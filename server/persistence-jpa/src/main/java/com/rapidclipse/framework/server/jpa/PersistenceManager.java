/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.jpa;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.PersistenceException;
import javax.persistence.SharedCacheMode;
import javax.servlet.ServletContext;

import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;


/**
 * @author XDEV Software
 *
 */
public interface PersistenceManager
{
	///////////////////////////////////////////////////////////////////////////
	// factory //
	/////////////////////////////////////////////////

	public static interface Factory
	{
		public PersistenceManager createPersistenceManager(final ServletContext context);

		///////////////////////////////////////////////////////////////////////////
		// implementation //
		/////////////////////////////////////////////////

		public static class Implementation implements Factory
		{
			@Override
			public PersistenceManager createPersistenceManager(final ServletContext context)
			{
				return new PersistenceManager.Implementation(context);
			}
		}
	}

	public static final String FACTORY_INIT_PARAMETER = "rap.persistenceManager.factory";

	public String getDefaultPersistenceUnit();

	public Iterable<String> getPersistenceUnits();

	public Iterable<Class<?>> getPersistenceUnitClasses(final String persistenceUnit);

	public String getPersistenceUnit(Class<?> managedType);

	public EntityManagerFactory getEntityManagerFactory(final String persistenceUnit);

	public default boolean isQueryCacheEnabled(final String persistenceUnit)
	{
		return isQueryCacheEnabled(getEntityManagerFactory(persistenceUnit));
	}

	public boolean isQueryCacheEnabled(final EntityManagerFactory factory);

	public default SharedCacheMode getQueryCacheMode(final String persistenceUnit)
	{
		return getQueryCacheMode(getEntityManagerFactory(persistenceUnit));
	}

	public SharedCacheMode getQueryCacheMode(final EntityManagerFactory factory);

	public void close();

	public static class Implementation implements PersistenceManager
	{
		private final Map<String, Collection<Class<?>>> unitToClasses;
		private final Map<Class<?>, String>             classToUnit;
		private final Map<String, EntityManagerFactory> entityManagerFactories = new HashMap<>();
		private Boolean                                 queryCacheEnabled;
		private SharedCacheMode                         queryCacheMode;

		public Implementation(final ServletContext servletContext) throws PersistenceException
		{
			this.unitToClasses = readPersistenceUnitTypes(servletContext);
			this.classToUnit   = createClassToUnitMap(this.unitToClasses);
		}

		protected Map<String, Collection<Class<?>>> readPersistenceUnitTypes(
			final ServletContext servletContext)
			throws PersistenceException
		{
			final Map<String, Collection<Class<?>>> persistenceUnitTypes = new LinkedHashMap<>();

			try
			{
				final URL url = findPersistenceXML(servletContext);
				if(url != null)
				{
					final ClassLoader classLoader = servletContext.getClassLoader();
					final Document    document    = new SAXReader().read(url);
					final Element     rootElement = document.getRootElement();
					if(rootElement != null)
					{
						for(final Object o : rootElement.elements("persistence-unit"))
						{
							final Element        unitElement = (Element)o;
							final String         name        = unitElement.attributeValue("name");
							final List<Class<?>> classes     = new ArrayList<>();
							for(final Object clazzElem : unitElement.elements("class"))
							{
								final String className = ((Element)clazzElem).getTextTrim();
								if(className.length() > 0)
								{
									classes.add(classLoader.loadClass(className));
								}
							}
							persistenceUnitTypes.put(name, classes);
						}
					}
				}
			}
			catch(final Exception e)
			{
				throw new PersistenceException(e);
			}

			return persistenceUnitTypes;
		}

		protected URL findPersistenceXML(final ServletContext servletContext)
			throws MalformedURLException
		{
			URL resourceUrl = servletContext.getResource("/META-INF/persistence.xml");
			if(resourceUrl == null)
			{
				final ClassLoader classLoader = servletContext.getClassLoader();
				resourceUrl = classLoader.getResource("META-INF/persistence.xml");
			}
			return resourceUrl;
		}

		protected Map<Class<?>, String> createClassToUnitMap(
			final Map<String, Collection<Class<?>>> unitToClasses)
		{
			final Map<Class<?>, String> classToUnit = new HashMap<>();
			unitToClasses.entrySet().forEach(entry -> {
				final String unit = entry.getKey();
				entry.getValue().forEach(clazz -> classToUnit.put(clazz, unit));
			});
			return classToUnit;
		}

		@Override
		public String getDefaultPersistenceUnit()
		{
			if(this.unitToClasses.isEmpty())
			{
				return null;
			}
			return getPersistenceUnits().iterator().next();
		}

		@Override
		public Iterable<String> getPersistenceUnits()
		{
			return this.unitToClasses.keySet();
		}

		@Override
		public Iterable<Class<?>> getPersistenceUnitClasses(final String persistenceUnit)
		{
			return this.unitToClasses.get(persistenceUnit);
		}

		@Override
		public String getPersistenceUnit(Class<?> managedType)
		{
			while(managedType != null && managedType != Object.class)
			{
				final String unit = this.classToUnit.get(managedType);
				if(unit != null)
				{
					return unit;
				}
				managedType = managedType.getSuperclass();
			}

			throw new IllegalArgumentException("Not a managed type: " + managedType.getName());
		}

		@Override
		public EntityManagerFactory getEntityManagerFactory(final String persistenceUnit)
		{
			EntityManagerFactory factory = this.entityManagerFactories.get(persistenceUnit);
			if(factory == null)
			{
				factory = Persistence.createEntityManagerFactory(persistenceUnit);
				this.entityManagerFactories.put(persistenceUnit, factory);
			}
			return factory;
		}

		@Override
		public boolean isQueryCacheEnabled(final EntityManagerFactory factory)
		{
			if(this.queryCacheEnabled == null)
			{
				final Map<String, Object> properties = factory.getProperties();
				final Object              property   = properties.get("hibernate.cache.use_query_cache");
				this.queryCacheEnabled = "true".equals(property);
			}

			return this.queryCacheEnabled;
		}

		@Override
		public SharedCacheMode getQueryCacheMode(final EntityManagerFactory factory)
		{
			if(this.queryCacheMode == null)
			{
				this.queryCacheMode = SharedCacheMode.ENABLE_SELECTIVE;

				final Map<String, Object> properties = factory.getProperties();
				final Object              property   = properties.get("rap.queryCache.mode");
				if(property != null)
				{
					try
					{
						this.queryCacheMode = SharedCacheMode.valueOf(property.toString());
					}
					catch(final IllegalArgumentException e)
					{
					}
				}
			}

			return this.queryCacheMode;
		}

		@Override
		public void close()
		{
			this.entityManagerFactories.values().forEach(factory -> {
				if(factory.isOpen())
				{
					factory.close();
				}
			});
			this.entityManagerFactories.clear();
		}
	}
}

/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
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

import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.Persistence;
import jakarta.persistence.PersistenceException;
import jakarta.persistence.SharedCacheMode;
import jakarta.servlet.ServletContext;

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
		
		public static class Default implements Factory
		{
			protected Default()
			{
				super();
			}
			
			@Override
			public PersistenceManager createPersistenceManager(final ServletContext context)
			{
				return PersistenceManager.New(context);
			}
		}
	}
	
	public static Factory Factory()
	{
		return new Factory.Default();
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
	
	public static PersistenceManager New(final ServletContext servletContext) throws PersistenceException
	{
		final Map<String, Collection<Class<?>>> unitToClasses = Static.readPersistenceUnitTypes(servletContext);
		return new Default(unitToClasses);
	}
	
	public static class Static
	{
		public static Map<String, Collection<Class<?>>> readPersistenceUnitTypes(
			final ServletContext servletContext)
			throws PersistenceException
		{
			final Map<String, Collection<Class<?>>> persistenceUnitTypes = new LinkedHashMap<>();
			
			// TODO: Replace SAXReader by javax.xml.parsers.DocumentBuilder & org.w3c.dom.Document 
			// as in org.hibernate.jpa.boot.internal.PersistenceXmlParser or 
			// use PersistenceXmlParser instead
			
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
		
		public static URL findPersistenceXML(final ServletContext servletContext)
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
		
		private Static()
		{
			throw new Error();
		}
	}
	
	public static class Default implements PersistenceManager
	{
		private final Map<String, Collection<Class<?>>> unitToClasses;
		private final Map<Class<?>, String>             classToUnit;
		private final Map<String, EntityManagerFactory> entityManagerFactories = new HashMap<>();
		private Boolean                                 queryCacheEnabled;
		private SharedCacheMode                         queryCacheMode;
		
		protected Default(final Map<String, Collection<Class<?>>> unitToClasses)
		{
			this.unitToClasses = unitToClasses;
			this.classToUnit   = createClassToUnitMap(unitToClasses);
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

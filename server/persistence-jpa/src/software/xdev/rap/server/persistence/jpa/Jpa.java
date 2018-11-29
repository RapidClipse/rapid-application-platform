/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.persistence.jpa;


import java.io.Serializable;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.persistence.Embeddable;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.EntityManager;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.persistence.SharedCacheMode;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Fetch;
import javax.persistence.criteria.FetchParent;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.Attribute;
import javax.persistence.metamodel.ManagedType;
import javax.persistence.metamodel.Metamodel;
import javax.persistence.metamodel.PluralAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.servlet.ServletContext;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.annotations.QueryHints;

import software.xdev.rap.server.RapServlet;
import software.xdev.rap.server.ReflectionUtils;
import software.xdev.rap.server.SoftCache;


/**
 * @author XDEV Software
 *
 */
public final class Jpa
{
	private final static String											HINT_CACHE_STORE_MODE		= "javax.persistence.cache.storeMode";
	private final static String											HINT_CACHE_RETRIEVE_MODE	= "javax.persistence.cache.retrieveMode";
	
	private static PersistenceManager									persistenceManager;
	private static SessionStrategyProvider								sessionStrategyProvider;
	
	private static final SoftCache<Class<?>, DataAccessObject<?, ?>>	daoCache					= new SoftCache<>();
	
	
	/**
	 * @return the persistenceManager
	 */
	public static PersistenceManager getPersistenceManager()
	{
		if(persistenceManager == null)
		{
			persistenceManager = createPersistenceManager(
					RapServlet.getRapServlet().getServletContext());
		}
		
		return persistenceManager;
	}
	
	
	private static PersistenceManager createPersistenceManager(final ServletContext context)
	{
		final String className = context
				.getInitParameter(PersistenceManager.FACTORY_INIT_PARAMETER);
		if(!StringUtils.isEmpty(className))
		{
			try
			{
				final PersistenceManager.Factory factory = (PersistenceManager.Factory)Class
						.forName(className).newInstance();
				return factory.createPersistenceManager(context);
			}
			catch(final Throwable t)
			{
				throw new RuntimeException(t);
			}
		}
		
		return new PersistenceManager.Implementation(context);
	}
	
	
	/**
	 * @return the sessionStrategyProvider
	 */
	public static SessionStrategyProvider getSessionStrategyProvider()
	{
		if(sessionStrategyProvider == null)
		{
			sessionStrategyProvider = createSessionStrategyProvider(
					RapServlet.getRapServlet().getServletContext());
		}
		
		return sessionStrategyProvider;
	}
	
	
	private static SessionStrategyProvider createSessionStrategyProvider(
			final ServletContext context)
	{
		final String className = context
				.getInitParameter(SessionStrategyProvider.FACTORY_INIT_PARAMETER);
		if(!StringUtils.isEmpty(className))
		{
			try
			{
				final SessionStrategyProvider.Factory factory = (SessionStrategyProvider.Factory)Class
						.forName(className).newInstance();
				return factory.createSessionStrategyProvider(context);
			}
			catch(final Throwable t)
			{
				throw new RuntimeException(t);
			}
		}

		return new SessionStrategyProvider.Implementation();
	}
	
	
	/**
	 *
	 * @param managedType
	 * @return
	 */
	public static String getPersistenceUnit(final Class<?> managedType)
	{
		final PersistenceManager persistenceManager;
		if((persistenceManager = getPersistenceManager()) != null)
		{
			return persistenceManager.getPersistenceUnit(managedType);
		}
		
		return null;
	}
	
	
	/**
	 *
	 * @param managedType
	 * @return
	 */
	public static EntityManager getEntityManager(final Class<?> managedType)
	{
		final String persistenceUnit;
		if((persistenceUnit = getPersistenceUnit(managedType)) != null)
		{
			return getEntityManager(persistenceUnit);
		}
		
		return null;
	}
	
	
	/**
	 *
	 * @param persistenceUnit
	 * @return
	 */
	public static EntityManager getEntityManager(final String persistenceUnit)
	{
		final Conversationables conversationables;
		if((conversationables = Conversationables.getCurrent()) != null)
		{
			final Conversationable conversationable;
			if((conversationable = conversationables.get(persistenceUnit)) != null)
			{
				return conversationable.getEntityManager();
			}
		}
		
		return null;
	}
	
	
	/**
	 *
	 * @param managedType
	 * @return
	 */
	public static <T> CriteriaQuery<T> createCriteriaQuery(final Class<T> managedType)
	{
		final EntityManager entityManager;
		if((entityManager = getEntityManager(managedType)) != null)
		{
			return entityManager.getCriteriaBuilder().createQuery(managedType);
		}
		
		return null;
	}
	
	
	public static <C> ManagedType<C> getManagedType(final Class<C> entityClass)
	{
		final EntityManager entityManager = getEntityManager(entityClass);
		if(entityManager == null)
		{
			throw new IllegalArgumentException("Not an entity class: " + entityClass.getName());
		}

		return entityManager.getMetamodel().managedType(entityClass);
	}


	public static Attribute<?, ?> resolveAttribute(final Class<?> entityClass,
			final String propertyPath)
	{
		final List<Attribute<?, ?>> attributes = resolveAttributes(entityClass,propertyPath);
		return attributes == null || attributes.isEmpty() ? null
				: attributes.get(attributes.size() - 1);
	}


	/**
	 * @since 3.0
	 */
	public static List<Attribute<?, ?>> resolveAttributes(final Class<?> entityClass,
			final String propertyPath)
	{
		try
		{
			final List<Attribute<?, ?>> attributes = new ArrayList<>();

			Class<?> current = entityClass;

			for(final String pathItem : propertyPath.split("\\."))
			{
				final Attribute<?, ?> attribute = getManagedType(current).getAttribute(pathItem);
				attributes.add(attribute);
				if(attribute instanceof PluralAttribute)
				{
					current = ((PluralAttribute<?, ?, ?>)attribute).getElementType().getJavaType();
				}
				else
				{
					current = attribute.getJavaType();
				}
			}

			return attributes;
		}
		catch(final IllegalArgumentException e)
		{
			// attribute not found or not a managed type, XWS-870
			return null;
		}
	}


	public static boolean isManaged(final Class<?> clazz)
	{
		return clazz.getAnnotation(Entity.class) != null
				|| clazz.getAnnotation(Embeddable.class) != null
				|| clazz.getAnnotation(MappedSuperclass.class) != null;
	}


	/**
	 * @since 3.0
	 */
	public static void verifyPath(final Attribute<?, ?>... path)
	{
		verifyPath(Arrays.asList(path));
	}


	/**
	 * @since 3.0
	 */
	@SuppressWarnings("rawtypes")
	public static void verifyPath(final List<Attribute<?, ?>> path)
	{
		final List<Attribute<?, ?>> attributes = new ArrayList<>(path);
		Class<?> from = null;
		if(attributes.get(0).isCollection())
		{
			from = ((PluralAttribute)attributes.get(0)).getElementType().getJavaType();
		}
		else
		{
			from = attributes.get(0).getJavaType();
		}
		attributes.remove(0);
		for(final Attribute<?, ?> attribute : attributes)
		{
			if(!attribute.getDeclaringType().getJavaType().isAssignableFrom(from))
			{
				throw new IllegalStateException("Wrong path.");
			}
			from = attribute.getJavaType();
		}
	}


	/**
	 * @since 3.0
	 */
	public static String toPath(final List<Attribute<?, ?>> attributes)
	{
		return attributes.stream().map(Attribute::getName).collect(Collectors.joining("."));
	}


	/**
	 * @since 3.0
	 */
	public static Attribute<?, ?> getIdAttribute(final Class<?> entityClass)
	{
		final ManagedType<?> managedType = getManagedType(entityClass);
		if(managedType != null)
		{
			for(final Attribute<?, ?> attribute : managedType.getAttributes())
			{
				final Member javaMember = attribute.getJavaMember();
				if(javaMember instanceof AnnotatedElement
						&& ((AnnotatedElement)javaMember).getAnnotation(Id.class) != null)
				{
					return attribute;
				}
			}
		}

		return null;
	}


	/**
	 * @since 3.0
	 */
	public static Attribute<?, ?> getEmbeddedIdAttribute(final Class<?> entityClass)
	{
		final ManagedType<?> managedType = getManagedType(entityClass);
		if(managedType != null)
		{
			for(final Attribute<?, ?> attribute : managedType.getAttributes())
			{
				final Member javaMember = attribute.getJavaMember();
				if(javaMember instanceof AnnotatedElement
						&& ((AnnotatedElement)javaMember).getAnnotation(EmbeddedId.class) != null)
				{
					return attribute;
				}
			}
		}

		return null;
	}
	
	
	/**
	 * @since 3.0
	 */
	public static <T, A> SingularAttribute<? super T, A> singularAttribute(
			final ManagedType<? super T> managedType, final Attribute<? super T, A> attribute)
	{
		return managedType.getSingularAttribute(attribute.getName(),attribute.getJavaType());
	}


	/**
	 * @since 3.0
	 */
	public static <T> SingularAttribute<? super T, String> stringAttribute(
			final ManagedType<? super T> managedType, final Attribute<? super T, ?> attribute)
	{
		return managedType.getSingularAttribute(attribute.getName(),String.class);
	}


	public static Predicate andPredicate(final CriteriaBuilder builder,
			final Predicate... predicatesNullAllowed)
	{
		return andPredicate(builder,Arrays.asList(predicatesNullAllowed));
	}
	
	
	public static Predicate andPredicate(final CriteriaBuilder builder,
			final Collection<Predicate> predicatesNullAllowed)
	{
		final List<Predicate> predicates = predicatesNullAllowed.stream().filter(Objects::nonNull)
				.collect(Collectors.toList());
		if(predicates == null || predicates.isEmpty())
		{
			return null;
		}
		else if(predicates.size() == 1)
		{
			return predicates.get(0);
		}
		else
		{
			return builder.and(predicates.toArray(new Predicate[predicates.size()]));
		}
	}
	
	
	public static Predicate orPredicate(final CriteriaBuilder builder,
			final Predicate... predicatesNullAllowed)
	{
		return orPredicate(builder,Arrays.asList(predicatesNullAllowed));
	}
	
	
	public static Predicate orPredicate(final CriteriaBuilder builder,
			final Collection<Predicate> predicatesNullAllowed)
	{
		final List<Predicate> predicates = predicatesNullAllowed.stream().filter(Objects::nonNull)
				.collect(Collectors.toList());
		if(predicates == null || predicates.isEmpty())
		{
			return null;
		}
		else if(predicates.size() == 1)
		{
			return predicates.get(0);
		}
		else
		{
			return builder.or(predicates.toArray(new Predicate[predicates.size()]));
		}
	}
	
	
	@SuppressWarnings("unchecked")
	public static <E, F> Path<F> getPath(final Root<E> root, final List<Attribute<?, ?>> attributes)
	{
		Path<?> path = root;
		for(final Attribute<?, ?> attribute : attributes)
		{
			boolean found = false;
			if(path instanceof FetchParent)
			{
				for(final Fetch<E, ?> fetch : ((FetchParent<?, E>)path).getFetches())
				{
					if(attribute.getName().equals(fetch.getAttribute().getName())
							&& (fetch instanceof Join<?, ?>))
					{
						path = (Join<E, ?>)fetch;
						found = true;
						break;
					}
				}
			}
			if(!found)
			{
				if(attribute instanceof PluralAttribute)
				{
					path = ((From<?, ?>)path).join(attribute.getName(),JoinType.LEFT);
				}
				else
				{
					path = path.get(attribute.getName());
				}
			}
		}
		return (Path<F>)path;
	}
	
	
	/**
	 * @since 4.0
	 */
	public static void applyCacheHints(final TypedQuery<?> query, final CacheableQuery.Kind kind,
			final Class<?> managedType)
	{
		applyCacheHints(query,getCacheableQueryAnnotation(managedType,kind),
				getPersistenceUnit(managedType));
	}
	
	
	/**
	 * @since 4.0
	 */
	public static void applyCacheHints(final TypedQuery<?> typedQuery,
			final CacheableQuery cacheableQuery, final String persistenceUnit)
	{
		boolean cacheable = false;
		
		final SharedCacheMode queryCacheMode = getPersistenceManager()
				.getQueryCacheMode(persistenceUnit);
		switch(queryCacheMode)
		{
			case ALL:
				cacheable = true;
			break;
		
			case NONE:
			case UNSPECIFIED:
				cacheable = false;
			break;
		
			case DISABLE_SELECTIVE:
				if(cacheableQuery != null)
				{
					cacheable = cacheableQuery.cache();
				}
				else
				{
					cacheable = true;
				}
			break;
		
			case ENABLE_SELECTIVE:
				if(cacheableQuery != null)
				{
					cacheable = cacheableQuery.cache();
				}
				else
				{
					cacheable = false;
				}
			break;
		}
		
		typedQuery.setHint(QueryHints.CACHEABLE,cacheable);
		
		if(cacheable && cacheableQuery != null)
		{
			final String region = cacheableQuery.region();
			if(!StringUtils.isBlank(region))
			{
				typedQuery.setHint(QueryHints.CACHE_REGION,region);
			}
			
			typedQuery.setHint(HINT_CACHE_STORE_MODE,cacheableQuery.storeMode());
			typedQuery.setHint(HINT_CACHE_RETRIEVE_MODE,cacheableQuery.retrieveMode());
		}
	}
	
	
	public static void reattachIfManaged(final Object bean)
	{
		if(isManaged(bean.getClass()))
		{
			getDao(bean).reattach(bean);
		}
	}
	
	
	public static <D extends DataAccessObject<?, ?>> D getDao(final Class<D> daoType)
			throws RuntimeException
	{
		synchronized(daoCache)
		{
			@SuppressWarnings("unchecked")
			D dao = (D)daoCache.get(daoType);
			
			if(dao == null)
			{
				try
				{
					dao = daoType.newInstance();
					daoCache.put(daoType,dao);
				}
				catch(InstantiationException | IllegalAccessException e)
				{
					throw new RuntimeException(e);
				}
			}
			
			return dao;
		}
	}
	
	
	@SuppressWarnings("unchecked")
	public static <T, I extends Serializable> DataAccessObject<T, I> getDao(final T entity)
			throws RuntimeException
	{
		return (DataAccessObject<T, I>)getDaoByEntityType(entity.getClass());
	}
	
	
	@SuppressWarnings("unchecked")
	public static <T, I extends Serializable> DataAccessObject<T, I> getDaoByEntityType(
			final Class<T> entity) throws RuntimeException
	{
		final DAO dao = entity.getAnnotation(DAO.class);
		if(dao == null)
		{
			throw new RuntimeException("Not an entity");
		}
		
		return (DataAccessObject<T, I>)getDao(dao.value());
	}
	
	
	public static CacheableQuery getCacheableQueryAnnotation(final Class<?> clazz,
			final CacheableQuery.Kind kind)
	{
		CacheableQuery cacheableQuery = clazz.getAnnotation(CacheableQuery.class);
		if(cacheableQuery != null && kind.equals(cacheableQuery.kind()))
		{
			return cacheableQuery;
		}
		
		final CacheableQueries cacheableQueries = clazz.getAnnotation(CacheableQueries.class);
		if(cacheableQueries != null)
		{
			cacheableQuery = Arrays.stream(cacheableQueries.value())
					.filter(query -> kind.equals(query.kind())).findAny().orElse(null);
			if(cacheableQuery != null)
			{
				return cacheableQuery;
			}
		}
		
		final Class<?> superclass = clazz.getSuperclass();
		if(superclass != null)
		{
			return getCacheableQueryAnnotation(superclass,kind);
		}
		
		return null;
	}
	
	
	public static String getEntityIdAttributeName(final Class<?> entityType)
	{
		final SingularAttribute<? extends Object, ?> idAttribute = getEntityIdAttribute(entityType);
		return idAttribute != null ? idAttribute.getName() : null;
	}


	public static Object getEntityIdAttributeValue(final Object entity)
	{
		final SingularAttribute<? extends Object, ?> idAttribute = getEntityIdAttribute(
				entity.getClass());
		return Optional.ofNullable(idAttribute).map(a -> a.getJavaMember())
				.map(m -> ReflectionUtils.getMemberValue(entity,m)).orElse(null);
	}


	@SuppressWarnings("unchecked")
	private static <C> SingularAttribute<C, ?> getEntityIdAttribute(final Class<C> entityClass)
	{
		final EntityManager entityManager = getEntityManager(entityClass);
		if(entityManager == null)
		{
			throw new IllegalArgumentException("Not an entity class: " + entityClass.getName());
		}

		final ManagedType<C> managedType = entityManager.getMetamodel().managedType(entityClass);
		if(managedType == null)
		{
			throw new IllegalArgumentException("Not an entity class: " + entityClass.getName());
		}

		return managedType.getAttributes().stream().filter(SingularAttribute.class::isInstance)
				.map(SingularAttribute.class::cast).filter(SingularAttribute::isId).findFirst()
				.orElse(null);
	}
	
	
	public static void preload(final Object bean, final String... requiredProperties)
	{
		for(final String property : requiredProperties)
		{
			final Object propertyValue = resolveAttributeValue(bean,property);
			if(propertyValue != null)
			{
				// force eager loading
				if(propertyValue instanceof Collection<?>)
				{
					final Collection<?> collection = (Collection<?>)propertyValue;
					for(final Object object : collection)
					{
						if(isManaged(object.getClass()))
						{
							getEntityIdAttributeValue(object);
						}
					}
				}
				else if(isManaged(propertyValue.getClass()))
				{
					getEntityIdAttributeValue(propertyValue);
				}
			}
		}
	}


	public static Object resolveAttributeValue(Object managedObject, final String propertyPath)
	{
		final EntityManager entityManager = getEntityManager(managedObject.getClass());
		if(entityManager == null)
		{
			return null;
		}

		Object propertyValue = null;

		final Metamodel metamodel = entityManager.getMetamodel();
		ManagedType<?> managedType = null;

		final String[] parts = propertyPath.split("\\.");
		for(int i = 0; i < parts.length; i++)
		{
			Class<?> managedClass = managedObject.getClass();
			try
			{
				managedType = metamodel.managedType(managedClass);
			}
			catch(final IllegalArgumentException e)
			{
				// not a managed type, XWS-870
				return null;
			}

			final String name = parts[i];
			Attribute<?, ?> attribute = null;
			try
			{
				attribute = managedType.getAttribute(name);
			}
			catch(final IllegalArgumentException e)
			{
				// attribute not found, XWS-870
				return null;
			}
			managedClass = attribute.getJavaType();
			if(managedClass == null)
			{
				return null;
			}
			if(isManaged(managedClass))
			{
				try
				{
					managedType = metamodel.managedType(managedClass);
				}
				catch(final IllegalArgumentException e)
				{
					// not a managed type, XWS-870
					return null;
				}
			}

			propertyValue = ReflectionUtils.getMemberValue(managedObject,attribute.getJavaMember());
			if(propertyValue != null && isManaged(propertyValue.getClass()))
			{
				managedObject = propertyValue;
			}
		}

		return propertyValue;
	}
	
	
	private Jpa()
	{
		throw new Error();
	}
}

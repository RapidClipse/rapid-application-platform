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

import java.io.Serializable;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

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
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Fetch;
import javax.persistence.criteria.FetchParent;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;
import javax.persistence.metamodel.Attribute;
import javax.persistence.metamodel.ManagedType;
import javax.persistence.metamodel.PluralAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.annotations.QueryHints;

import com.rapidclipse.framework.server.RapServlet;
import com.rapidclipse.framework.server.jpa.dal.CacheableQueries;
import com.rapidclipse.framework.server.jpa.dal.CacheableQuery;
import com.rapidclipse.framework.server.jpa.dal.DAO;
import com.rapidclipse.framework.server.jpa.dal.DataAccessObject;
import com.rapidclipse.framework.server.util.ReflectionUtils;
import com.rapidclipse.framework.server.util.SoftCache;


/**
 * @author XDEV Software
 *
 */
public final class Jpa
{
	private final static String HINT_CACHE_STORE_MODE    =
		"javax.persistence.cache.storeMode";
	private final static String HINT_CACHE_RETRIEVE_MODE =
		"javax.persistence.cache.retrieveMode";
	
	public final static String  PROPERTY_SEPARATOR       = ".";
	private final static String PROPERTY_SEPARATOR_REGEX = "\\.";
	
	private static PersistenceManager persistenceManager;
	
	private static SessionStrategyProvider sessionStrategyProvider;
	
	private final static SoftCache<Class<?>, DataAccessObject<?, ?>> daoCache = new SoftCache<>();
	
	private final static AtomicLong aliasCounter = new AtomicLong();
	
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
		return getPersistenceManager().getPersistenceUnit(managedType);
	}
	
	/**
	 *
	 * @param managedType
	 * @return
	 */
	public static EntityManager getEntityManager(final Class<?> managedType)
	{
		return getEntityManager(getPersistenceUnit(managedType));
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
		
		throw new IllegalStateException("No active conversation found.");
	}
	
	public static <T> Long count(final CriteriaQuery<T> criteria, final EntityManager entityManager)
	{
		return entityManager.createQuery(countCriteria(criteria, entityManager)).getSingleResult();
	}
	
	public static <T> CriteriaQuery<Long> countCriteria(
		final CriteriaQuery<T> criteria,
		final EntityManager entityManager)
	{
		final CriteriaBuilder     builder       = entityManager.getCriteriaBuilder();
		final CriteriaQuery<Long> countCriteria = builder.createQuery(Long.class);
		copyCriteriaWithoutSelectionAndOrder(criteria, countCriteria, false);
		
		Expression<Long> countExpression;
		
		if(criteria.isDistinct())
		{
			countExpression = builder
				.countDistinct(findRoot(countCriteria, criteria.getResultType()));
		}
		else
		{
			countExpression = builder.count(findRoot(countCriteria, criteria.getResultType()));
		}
		
		return countCriteria.select(countExpression);
	}
	
	public static <T> void copyCriteria(final CriteriaQuery<T> from, final CriteriaQuery<T> to)
	{
		copyCriteriaWithoutSelection(from, to);
		to.select(from.getSelection());
	}
	
	public static void copyCriteriaWithoutSelection(
		final CriteriaQuery<?> from,
		final CriteriaQuery<?> to)
	{
		copyCriteriaWithoutSelectionAndOrder(from, to, true);
		to.orderBy(from.getOrderList());
	}
	
	public static void copyCriteriaWithoutSelectionAndOrder(
		final CriteriaQuery<?> from,
		final CriteriaQuery<?> to,
		final boolean copyFetches)
	{
		for(final Root<?> root : from.getRoots())
		{
			final Root<?> dest = to.from(root.getJavaType());
			dest.alias(getOrCreateAlias(root));
			copyJoins(root, dest);
			if(copyFetches)
			{
				copyFetches(root, dest);
			}
		}
		
		to.groupBy(from.getGroupList());
		to.distinct(from.isDistinct());
		
		if(from.getGroupRestriction() != null)
		{
			to.having(from.getGroupRestriction());
		}
		
		final Predicate predicate = from.getRestriction();
		if(predicate != null)
		{
			to.where(predicate);
		}
	}
	
	public static <T> Root<T> findRoot(final CriteriaQuery<?> query, final Class<T> clazz)
	{
		for(final Root<?> r : query.getRoots())
		{
			if(clazz.equals(r.getJavaType()))
			{
				return (Root<T>)r.as(clazz);
			}
		}
		return null;
	}
	
	public static <T> String getOrCreateAlias(final Selection<T> selection)
	{
		String alias = selection.getAlias();
		if(alias == null)
		{
			alias = "__generatedAlias" + aliasCounter.incrementAndGet();
			selection.alias(alias);
		}
		return alias;
	}
	
	public static void copyJoins(final From<?, ?> from, final From<?, ?> to)
	{
		for(final Join<?, ?> j : from.getJoins())
		{
			final Join<?, ?> toJoin = to.join(j.getAttribute().getName(), j.getJoinType());
			toJoin.alias(getOrCreateAlias(j));
			
			copyJoins(j, toJoin);
		}
	}
	
	public static void copyFetches(final From<?, ?> from, final From<?, ?> to)
	{
		for(final Fetch<?, ?> f : from.getFetches())
		{
			final Fetch<?, ?> toFetch = to.fetch(f.getAttribute().getName());
			copyFetches(f, toFetch);
		}
	}
	
	public static void copyFetches(final Fetch<?, ?> from, final Fetch<?, ?> to)
	{
		for(final Fetch<?, ?> f : from.getFetches())
		{
			final Fetch<?, ?> toFetch = to.fetch(f.getAttribute().getName());
			copyFetches(f, toFetch);
		}
	}
	
	public static Predicate andPredicate(
		final CriteriaBuilder builder,
		final Predicate... predicatesNullAllowed)
	{
		return andPredicate(builder, Arrays.asList(predicatesNullAllowed));
	}
	
	public static Predicate andPredicate(
		final CriteriaBuilder builder,
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
	
	public static Predicate orPredicate(
		final CriteriaBuilder builder,
		final Predicate... predicatesNullAllowed)
	{
		return orPredicate(builder, Arrays.asList(predicatesNullAllowed));
	}
	
	public static Predicate orPredicate(
		final CriteriaBuilder builder,
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
	
	public static <C> ManagedType<C> getManagedType(final Class<C> entityClass)
	{
		final EntityManager entityManager = getEntityManager(entityClass);
		if(entityManager == null)
		{
			throw new IllegalArgumentException("Not an entity class: " + entityClass.getName());
		}
		
		return entityManager.getMetamodel().managedType(entityClass);
	}
	
	public static Attribute<?, ?> resolveAttribute(
		final Class<?> entityClass,
		final String propertyPath)
	{
		return resolveAttributeChain(entityClass, propertyPath).last();
	}
	
	@SuppressWarnings("unchecked")
	public static <X> AttributeChain<X, ?> resolveAttributeChain(
		final Class<X> entityClass,
		final String propertyPath)
	{
		final List<Attribute<?, ?>> attributes = new ArrayList<>();
		
		Class<?> current = entityClass;
		
		for(final String name : propertyPath.split(PROPERTY_SEPARATOR_REGEX))
		{
			final Attribute<?, ?> attribute = getManagedType(current).getAttribute(name);
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
		
		return (AttributeChain<X, ?>)AttributeChain.New(attributes);
	}
	
	public static boolean isManaged(final Class<?> clazz)
	{
		return clazz.getAnnotation(Entity.class) != null
			|| clazz.getAnnotation(Embeddable.class) != null
			|| clazz.getAnnotation(MappedSuperclass.class) != null;
	}
	
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
	
	public static <T, A> SingularAttribute<? super T, A> singularAttribute(
		final ManagedType<? super T> managedType,
		final Attribute<? super T, A> attribute)
	{
		return managedType.getSingularAttribute(attribute.getName(), attribute.getJavaType());
	}
	
	public static <T> SingularAttribute<? super T, String> stringAttribute(
		final ManagedType<? super T> managedType,
		final Attribute<? super T, ?> attribute)
	{
		return managedType.getSingularAttribute(attribute.getName(), String.class);
	}
	
	public static <E, F> Path<F> resolvePath(
		final Root<E> root,
		final Attribute<?, ?>... attributes)
	{
		return resolvePath(root, Arrays.asList(attributes));
	}
	
	@SuppressWarnings("unchecked")
	public static <E, F> Path<F> resolvePath(
		final Root<E> root,
		final Iterable<? extends Attribute<?, ?>> attributes)
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
						path  = (Join<E, ?>)fetch;
						found = true;
						break;
					}
				}
			}
			if(!found)
			{
				if(attribute instanceof PluralAttribute)
				{
					path = ((From<?, ?>)path).join(attribute.getName(), JoinType.LEFT);
				}
				else
				{
					path = path.get(attribute.getName());
				}
			}
		}
		return (Path<F>)path;
	}
	
	@SuppressWarnings("unchecked")
	public static <T> Path<T> resolvePath(final Path<?> path, final String propertyPath)
	{
		if(StringUtils.isEmpty(propertyPath))
		{
			return (Path<T>)path;
		}
		
		final String name = StringUtils.substringBefore(propertyPath, PROPERTY_SEPARATOR);
		return resolvePath(path.get(name), StringUtils.substringAfter(propertyPath, PROPERTY_SEPARATOR));
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
			.map(m -> ReflectionUtils.getMemberValue(entity, m)).orElse(null);
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
	
	public static void applyCacheHints(
		final TypedQuery<?> query,
		final CacheableQuery.Kind kind,
		final Class<?> managedType)
	{
		applyCacheHints(query, getCacheableQueryAnnotation(managedType, kind),
			getPersistenceUnit(managedType));
	}
	
	public static void applyCacheHints(
		final TypedQuery<?> typedQuery,
		final CacheableQuery cacheableQuery,
		final String persistenceUnit)
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
		
		typedQuery.setHint(QueryHints.CACHEABLE, cacheable);
		
		if(cacheable && cacheableQuery != null)
		{
			final String region = cacheableQuery.region();
			if(!StringUtils.isBlank(region))
			{
				typedQuery.setHint(QueryHints.CACHE_REGION, region);
			}
			
			typedQuery.setHint(HINT_CACHE_STORE_MODE, cacheableQuery.storeMode());
			typedQuery.setHint(HINT_CACHE_RETRIEVE_MODE, cacheableQuery.retrieveMode());
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
					daoCache.put(daoType, dao);
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
		final Class<T> entity)
		throws RuntimeException
	{
		final DAO dao = entity.getAnnotation(DAO.class);
		if(dao == null)
		{
			throw new RuntimeException("Not an entity");
		}
		
		return (DataAccessObject<T, I>)getDao(dao.value());
	}
	
	public static CacheableQuery getCacheableQueryAnnotation(
		final Class<?> clazz,
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
			return getCacheableQueryAnnotation(superclass, kind);
		}
		
		return null;
	}
	
	public static void preload(final Object entity, final String... requiredProperties)
	{
		for(final String property : requiredProperties)
		{
			final Object propertyValue = resolveValue(entity, property);
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
	
	public static String toPropertyPath(final Attribute<?, ?>... attributeChain)
	{
		if(attributeChain.length == 1)
		{
			return attributeChain[0].getName();
		}
		
		return Arrays.stream(attributeChain).map(Attribute::getName)
			.collect(Collectors.joining(PROPERTY_SEPARATOR));
	}
	
	public static String toPropertyPath(final Iterable<? extends Attribute<?, ?>> attributeChain)
	{
		final Stream<? extends Attribute<?, ?>> stream =
			attributeChain instanceof Collection ? ((Collection<? extends Attribute<?, ?>>)attributeChain).stream()
				: StreamSupport.stream(attributeChain.spliterator(), false);
		return stream.map(Attribute::getName)
			.collect(Collectors.joining(PROPERTY_SEPARATOR));
	}
	
	public static <T> T resolveValue(final Object entity, final String propertyPath)
	{
		return resolveValue(entity, resolveAttributeChain(entity.getClass(), propertyPath));
	}
	
	@SuppressWarnings("unchecked")
	public static <T> T resolveValue(final Object entity, final Attribute<?, ?>... attributeChain)
	{
		return (T)resolveValue(entity, Arrays.asList(attributeChain));
	}
	
	@SuppressWarnings({"rawtypes", "unchecked"})
	public static <T> T resolveValue(
		final Object entity,
		final Iterable<? extends Attribute<?, ?>> attributeChain)
	{
		Object current = entity;
		for(final Attribute attribute : attributeChain)
		{
			current = resolveValue(current, attribute);
		}
		return (T)current;
	}
	
	@SuppressWarnings("unchecked")
	public static <X, Y> Y resolveValue(final X entity, final Attribute<X, Y> attribute)
	{
		return (Y)ReflectionUtils.getMemberValue(entity, attribute.getJavaMember());
	}
	
	@WebListener
	public static class ContextListener implements ServletContextListener
	{
		@Override
		public void contextInitialized(final ServletContextEvent sce)
		{
		}
		
		@Override
		public void contextDestroyed(final ServletContextEvent sce)
		{
			if(Jpa.persistenceManager != null)
			{
				Jpa.persistenceManager.close();
				Jpa.persistenceManager = null;
			}
		}
	}
	
	private Jpa()
	{
		throw new Error();
	}
}

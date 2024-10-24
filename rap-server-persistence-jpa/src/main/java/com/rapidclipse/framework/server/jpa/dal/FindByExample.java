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
package com.rapidclipse.framework.server.jpa.dal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.cfg.AvailableSettings;
import org.hibernate.jpa.AvailableHints;

import com.rapidclipse.framework.server.jpa.AttributeChain;
import com.rapidclipse.framework.server.jpa.Jpa;
import com.rapidclipse.framework.server.util.ReflectionUtils;

import jakarta.persistence.EntityManager;
import jakarta.persistence.Query;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Fetch;
import jakarta.persistence.criteria.FetchParent;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.ListJoin;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.metamodel.Attribute;
import jakarta.persistence.metamodel.Attribute.PersistentAttributeType;
import jakarta.persistence.metamodel.EntityType;
import jakarta.persistence.metamodel.ManagedType;
import jakarta.persistence.metamodel.PluralAttribute;
import jakarta.persistence.metamodel.SingularAttribute;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * @author XDEV Software
 */
class FindByExample<E>
{
	private static final Logger LOG = LoggerFactory.getLogger(FindByExample.class);
	private final Class<E>         persistentClass;
	private final EntityManager    entityManager;
	private final SearchParameters searchParameters;

	public FindByExample(
		final Class<E> persistentClass,
		final EntityManager entityManager,
		final SearchParameters searchParameters)
	{
		this.persistentClass  = persistentClass;
		this.entityManager    = entityManager;
		this.searchParameters = searchParameters;
	}

	public List<E> findByExample(final E entity)
	{
		final CriteriaBuilder builder       = this.entityManager.getCriteriaBuilder();
		CriteriaQuery<E>      criteriaQuery = builder.createQuery(this.persistentClass);

		if(this.searchParameters.getDistinct())
		{
			criteriaQuery.distinct(true);
		}

		final Root<E> root = criteriaQuery.from(this.persistentClass);

		final Predicate predicate = getPredicate(criteriaQuery, root, builder, entity);
		if(predicate != null)
		{
			criteriaQuery = criteriaQuery.where(predicate);
		}

		fetches(root);

		criteriaQuery.orderBy(buildJpaOrders(this.searchParameters.getOrders(), root, builder));

		final TypedQuery<E> typedQuery = this.entityManager.createQuery(criteriaQuery);
		applyCacheHints(typedQuery);
		applyPagination(typedQuery);
		return typedQuery.getResultList();
	}

	public int countByExample(final E entity)
	{
		final CriteriaBuilder builder = this.entityManager.getCriteriaBuilder();

		CriteriaQuery<Long> criteriaQuery = builder.createQuery(Long.class);
		final Root<E>       root          = criteriaQuery.from(this.persistentClass);

		if(this.searchParameters.getDistinct())
		{
			criteriaQuery = criteriaQuery.select(builder.countDistinct(root));
		}
		else
		{
			criteriaQuery = criteriaQuery.select(builder.count(root));
		}

		// predicate
		final Predicate predicate = getPredicate(criteriaQuery, root, builder, entity);
		if(predicate != null)
		{
			criteriaQuery = criteriaQuery.where(predicate);
		}

		// construct order by to fetch or joins if needed
		buildJpaOrders(this.searchParameters.getOrders(), root, builder);

		final TypedQuery<Long> typedQuery = this.entityManager.createQuery(criteriaQuery);
		applyCacheHints(typedQuery);
		return typedQuery.getSingleResult().intValue();
	}

	private Predicate getPredicate(
		final CriteriaQuery<?> criteriaQuery,
		final Root<E> root,
		final CriteriaBuilder builder,
		final E entity)
	{
		return Jpa.andPredicate(builder, //
			bySearchPredicate(root, builder, entity), //
			byPredicate(criteriaQuery, root, builder, entity));
	}

	private Predicate bySearchPredicate(
		final Root<E> root,
		final CriteriaBuilder builder,
		final E entity)
	{
		return concatPredicate(builder, byRanges(root, builder), //
			byPropertySelectors(root, builder), //
			byExample(root, builder, entity), //
			byPattern(root, builder, this.persistentClass));
	}

	private Predicate concatPredicate(
		final CriteriaBuilder builder,
		final Predicate... predicatesNullAllowed)
	{
		return concatPredicate(builder, Arrays.asList(predicatesNullAllowed));
	}

	private Predicate concatPredicate(
		final CriteriaBuilder builder,
		final Collection<Predicate> predicatesNullAllowed)
	{
		if(this.searchParameters.isAndMode())
		{
			return Jpa.andPredicate(builder, predicatesNullAllowed);
		}
		else
		{
			return Jpa.orPredicate(builder, predicatesNullAllowed);
		}
	}

	@SuppressWarnings("unchecked")
	private Predicate byRanges(final Root<E> root, final CriteriaBuilder builder)
	{
		final Collection<Range<?, ?>> ranges     = this.searchParameters.getRanges();
		final List<Predicate>         predicates = new ArrayList<>();
		for(final Range<?, ?> r : ranges)
		{
			final Range<E, ?> range = (Range<E, ?>)r;
			if(range.isSet())
			{
				final Predicate rangePredicate = buildRangePredicate(range, root, builder);
				if(rangePredicate != null)
				{
					predicates.add(rangePredicate);
				}
			}
		}

		return concatPredicate(builder, predicates);
	}

	private <D extends Comparable<? super D>> Predicate buildRangePredicate(
		final Range<E, D> range,
		final Root<E> root,
		final CriteriaBuilder builder)
	{
		Predicate     rangePredicate = null;
		final Path<D> path           = Jpa.resolvePath(root, range.getAttributes());
		if(range.isBetween())
		{
			rangePredicate = builder.between(path, range.getFrom(), range.getTo());
		}
		else if(range.isFromSet())
		{
			rangePredicate = builder.greaterThanOrEqualTo(path, range.getFrom());
		}
		else if(range.isToSet())
		{
			rangePredicate = builder.lessThanOrEqualTo(path, range.getTo());
		}

		if(rangePredicate != null)
		{
			if(!range.isIncludeNullSet() || range.getIncludeNull() == Boolean.FALSE)
			{
				return rangePredicate;
			}
			else
			{
				return builder.or(rangePredicate, builder.isNull(path));
			}
		}
		else
		{
			// no from/to is set, but include null or not could be:
			if(Boolean.TRUE == range.getIncludeNull())
			{
				return builder.isNull(path);
			}
			else if(Boolean.FALSE == range.getIncludeNull())
			{
				return builder.isNotNull(path);
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	private Predicate byPropertySelectors(final Root<E> root, final CriteriaBuilder builder)
	{
		final List<Predicate> predicates = new ArrayList<>();

		for(final PropertySelector<?, ?> selector : this.searchParameters.getProperties())
		{
			if(selector.isBoolean())
			{
				byBooleanSelector(root, builder, predicates,
					(PropertySelector<? super E, Boolean>)selector);
			}
			else if(selector.isString())
			{
				byStringSelector(root, builder, predicates,
					(PropertySelector<? super E, String>)selector);
			}
			else
			{
				byObjectSelector(root, builder, predicates, (PropertySelector<? super E, ?>)selector);
			}
		}
		return concatPredicate(builder, predicates);
	}

	private void byBooleanSelector(
		final Root<E> root,
		final CriteriaBuilder builder,
		final List<Predicate> predicates,
		final PropertySelector<? super E, Boolean> selector)
	{
		if(selector.isNotEmpty())
		{
			final List<Predicate> selectorPredicates = new ArrayList<>();

			for(final Boolean selection : selector.getSelected())
			{
				final Path<Boolean> path = Jpa.resolvePath(root, selector.getAttributes());
				if(selection == null)
				{
					selectorPredicates.add(builder.isNull(path));
				}
				else
				{
					selectorPredicates
						.add(selection ? builder.isTrue(path) : builder.isFalse(path));
				}
			}
			if(selector.isOrMode())
			{
				predicates.add(Jpa.orPredicate(builder, selectorPredicates));
			}
			else
			{
				predicates.add(Jpa.andPredicate(builder, selectorPredicates));
			}
		}
	}

	private void byStringSelector(
		final Root<E> root,
		final CriteriaBuilder builder,
		final List<Predicate> predicates,
		final PropertySelector<? super E, String> selector)
	{
		if(selector.isNotEmpty())
		{
			final List<Predicate> selectorPredicates = new ArrayList<>();

			for(final String selection : selector.getSelected())
			{
				final Path<String> path = Jpa.resolvePath(root, selector.getAttributes());
				selectorPredicates
					.add(stringPredicate(path, selection, selector.getSearchMode(), builder));
			}
			if(selector.isOrMode())
			{
				predicates.add(Jpa.orPredicate(builder, selectorPredicates));
			}
			else
			{
				predicates.add(Jpa.andPredicate(builder, selectorPredicates));
			}
		}
	}

	private Predicate stringPredicate(
		final Expression<String> path,
		final Object attrValue,
		final CriteriaBuilder builder)
	{
		return stringPredicate(path, attrValue, null, builder);
	}

	private Predicate stringPredicate(
		Expression<String> path,
		Object attrValue,
		final SearchMode searchMode,
		final CriteriaBuilder builder)
	{
		if(this.searchParameters.isCaseInsensitive())
		{
			path      = builder.lower(path);
			attrValue = ((String)attrValue).toLowerCase();
		}

		switch(searchMode != null ? searchMode : this.searchParameters.getSearchMode())
		{
			case EQUALS:
				return builder.equal(path, attrValue);
			case ENDING_LIKE:
				return builder.like(path, "%" + attrValue);
			case STARTING_LIKE:
				return builder.like(path, attrValue + "%");
			case ANYWHERE:
				return builder.like(path, "%" + attrValue + "%");
			case LIKE:
				return builder.like(path, (String)attrValue); // assume user
																// provide the
																// wild cards
			default:
				throw new IllegalStateException("expecting a search mode!");
		}
	}

	private void byObjectSelector(
		final Root<E> root,
		final CriteriaBuilder builder,
		final List<Predicate> predicates,
		final PropertySelector<? super E, ?> selector)
	{
		if(selector.isNotEmpty())
		{
			if(selector.isOrMode())
			{
				byObjectOrModeSelector(root, builder, predicates, selector);
			}
			else
			{
				byObjectAndModeSelector(root, builder, predicates, selector);
			}
		}
		else if(selector.isNotIncludingNullSet())
		{
			predicates.add(builder.isNotNull(Jpa.resolvePath(root, selector.getAttributes())));
		}
	}

	private void byObjectOrModeSelector(
		final Root<E> root,
		final CriteriaBuilder builder,
		final List<Predicate> predicates,
		final PropertySelector<? super E, ?> selector)
	{
		final List<Predicate> selectorPredicates = new ArrayList<>();
		final Path<?>         path               = Jpa.resolvePath(root, selector.getAttributes());
		List<?>               selected           = selector.getSelected();
		if(selected.contains(null))
		{
			selected = new ArrayList<>(selector.getSelected());
			selected.remove(null);
			selectorPredicates.add(builder.isNull(path));
		}
		if(!selected.isEmpty())
		{
			selectorPredicates.add(path.in(selected));
		}
		predicates.add(Jpa.orPredicate(builder, selectorPredicates));
	}

	private void byObjectAndModeSelector(
		final Root<E> root,
		final CriteriaBuilder builder,
		final List<Predicate> predicates,
		final PropertySelector<? super E, ?> selector)
	{
		final List<Predicate> selectorPredicates = new ArrayList<>();
		List<?>               selected           = selector.getSelected();
		if(selected.contains(null))
		{
			selected = new ArrayList<>(selector.getSelected());
			selected.remove(null);
			selectorPredicates.add(builder.isNull(Jpa.resolvePath(root, selector.getAttributes())));
		}
		for(final Object selection : selected)
		{
			final Path<?> path = Jpa.resolvePath(root, selector.getAttributes());
			selectorPredicates.add(builder.equal(path, selection));
		}
		predicates.add(Jpa.andPredicate(builder, selectorPredicates));
	}

	private Predicate byPredicate(
		final CriteriaQuery<?> criteriaQuery,
		final Root<E> root,
		final CriteriaBuilder builder,
		final E entity)
	{
		final PredicateSupplier mandatoryPredicateSupplier = this.searchParameters
			.getPredicateSupplier();
		if(mandatoryPredicateSupplier != null)
		{
			return mandatoryPredicateSupplier.getPredicate(criteriaQuery, root, builder, entity);
		}

		return null;
	}

	private Predicate byExample(final Root<E> root, final CriteriaBuilder builder, final E entity)
	{
		if(entity == null)
		{
			return null;
		}

		final Class<E>       type = root.getModel().getBindableJavaType();
		final ManagedType<E> mt   = this.entityManager.getMetamodel().entity(type);

		final List<Predicate> predicates = new ArrayList<>();
		predicates.addAll(byExample(mt, root, entity, builder));
		predicates.addAll(byExampleOnCompositePk(root, entity, builder));
		predicates.addAll(byExampleOnXToOne(mt, root, entity, builder));
		predicates.addAll(byExampleOnXToMany(mt, root, entity, builder));
		return concatPredicate(builder, predicates);
	}

	private List<Predicate> byExampleOnCompositePk(
		final Root<E> root,
		final E entity,
		final CriteriaBuilder builder)
	{
		final Attribute<?, ?> embeddedIdAttribute = Jpa.getEmbeddedIdAttribute(entity.getClass());
		if(embeddedIdAttribute == null)
		{
			return Collections.emptyList();
		}
		else
		{
			@SuppressWarnings("unchecked")
			final E id = (E)ReflectionUtils.getMemberValue(entity,
				embeddedIdAttribute.getJavaMember());
			return Arrays.asList(
				byExampleOnEmbeddable(root.get(embeddedIdAttribute.getName()), id, builder));
		}
	}

	private <EID> Predicate byExampleOnEmbeddable(
		final Path<EID> embeddablePath,
		final EID embeddableValue,
		final CriteriaBuilder builder)
	{
		if(embeddableValue == null)
		{
			return null;
		}

		final Class<EID>       type = embeddablePath.getModel().getBindableJavaType();
		final ManagedType<EID> mt   = this.entityManager.getMetamodel().embeddable(type);

		return Jpa.andPredicate(builder, byExample(mt, embeddablePath, embeddableValue, builder));
	}

	/*
	 * Add a predicate for each simple property whose value is not null.
	 */
	@SuppressWarnings("hiding")
	private <E> List<Predicate> byExample(
		final ManagedType<E> mt,
		final Path<E> mtPath,
		final E mtValue,
		final CriteriaBuilder builder)
	{
		final List<Predicate> predicates = new ArrayList<>();
		for(final SingularAttribute<? super E, ?> attr : mt.getSingularAttributes())
		{
			if(attr.getPersistentAttributeType() == PersistentAttributeType.MANY_TO_ONE //
				|| attr.getPersistentAttributeType() == PersistentAttributeType.ONE_TO_ONE //
				|| attr.getPersistentAttributeType() == PersistentAttributeType.EMBEDDED)
			{
				continue;
			}

			final Object attrValue = ReflectionUtils.getMemberValue(mtValue, attr.getJavaMember());
			if(attrValue != null)
			{
				if(attr.getJavaType() == String.class)
				{
					if(StringUtils.isNotEmpty((String)attrValue))
					{
						predicates.add(stringPredicate(mtPath.get(Jpa.stringAttribute(mt, attr)),
							attrValue, builder));
					}
				}
				else
				{
					predicates.add(
						builder.equal(mtPath.get(Jpa.singularAttribute(mt, attr)), attrValue));
				}
			}
		}
		return predicates;
	}

	/*
	 * Invoke byExample method for each not null x-to-one association when their
	 * pk is not set. This allows you to search entities based on an associated
	 * entity's properties value.
	 */
	@SuppressWarnings("unchecked")
	private <T, M2O> List<Predicate> byExampleOnXToOne(
		final ManagedType<T> mt,
		final Root<T> mtPath,
		final T mtValue,
		final CriteriaBuilder builder)
	{
		final List<Predicate> predicates = new ArrayList<>();
		for(final SingularAttribute<? super T, ?> attr : mt.getSingularAttributes())
		{
			if(attr.getPersistentAttributeType() == PersistentAttributeType.MANY_TO_ONE
				|| attr.getPersistentAttributeType() == PersistentAttributeType.ONE_TO_ONE)
			{
				final M2O              m2oValue = (M2O)ReflectionUtils.getMemberValue(mtValue,
					mt.getAttribute(attr.getName()).getJavaMember());
				final Class<M2O>       m2oType  = (Class<M2O>)attr.getBindableJavaType();
				final Path<M2O>        m2oPath  = (Path<M2O>)mtPath.get(attr);
				final ManagedType<M2O> m2oMt    = this.entityManager.getMetamodel().entity(m2oType);
				if(m2oValue != null)
				{
					final Object id = getIdValue(mtValue);
					if(id != null)
					{ // we have an id, let's restrict only on this field
						predicates.add(builder.equal(m2oPath.get("id"), id));
					}
					else
					{
						predicates.addAll(byExample(m2oMt, m2oPath, m2oValue, builder));
					}
				}
			}
		}
		return predicates;
	}

	private Object getIdValue(final Object entity)
	{
		final Attribute<?, ?> idAttribute = Jpa.getIdAttribute(entity.getClass());
		if(idAttribute != null)
		{
			return ReflectionUtils.getMemberValue(entity, idAttribute.getJavaMember());
		}
		return null;
	}

	/*
	 * Construct a join predicate on collection (eg many to many, List)
	 */
	private <T> List<Predicate> byExampleOnXToMany(
		final ManagedType<T> mt,
		final Root<T> mtPath,
		final T mtValue,
		final CriteriaBuilder builder)
	{
		final List<Predicate> predicates = new ArrayList<>();
		for(final PluralAttribute<? super T, ?, ?> pa : mt.getPluralAttributes())
		{
			if(pa.getCollectionType() == PluralAttribute.CollectionType.LIST)
			{
				final List<?> values = (List<?>)ReflectionUtils.getMemberValue(mtValue,
					mt.getAttribute(pa.getName()).getJavaMember());
				if(values != null && !values.isEmpty())
				{
					if(this.searchParameters.getUseAndInXToMany())
					{
						if(values.size() > 3)
						{
							LOG.warn(
								"Please note that using AND restriction on an Many to Many relationship requires as many joins as values");
						}
						for(final Object value : values)
						{
							final ListJoin<T, ?> join = mtPath.join(mt.getList(pa.getName()));
							predicates.add(join.in(value));
						}
					}
					else
					{
						final ListJoin<T, ?> join = mtPath.join(mt.getList(pa.getName()));
						predicates.add(join.in(values));
					}
				}
			}
		}
		return predicates;
	}

	@SuppressWarnings("unchecked")
	private Predicate byPattern(
		final Root<E> root,
		final CriteriaBuilder builder,
		final Class<E> type)
	{
		if(!this.searchParameters.hasSearchPattern())
		{
			return null;
		}

		final List<Predicate> predicates = new ArrayList<>();
		final EntityType<E>   entity     = this.entityManager.getMetamodel().entity(type);
		final String          pattern    = this.searchParameters.getSearchPattern();

		for(final SingularAttribute<? super E, ?> attr : entity.getSingularAttributes())
		{
			if(attr.getPersistentAttributeType() == PersistentAttributeType.MANY_TO_ONE
				|| attr.getPersistentAttributeType() == PersistentAttributeType.ONE_TO_ONE)
			{
				continue;
			}

			if(attr.getJavaType() == String.class)
			{
				predicates.add(stringPredicate(
					(Expression<String>)root.get(Jpa.singularAttribute(entity, attr)), pattern,
					builder));
			}
		}

		return Jpa.orPredicate(builder, predicates);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private void fetches(final Root<E> root)
	{
		for(final AttributeChain<?, ?> chain : this.searchParameters.getFetches())
		{
			FetchParent<?, ?> from = root;
			for(final Attribute<?, ?> attribute : chain)
			{
				boolean found = false;
				for(final Fetch<?, ?> fetch : from.getFetches())
				{
					if(attribute.equals(fetch.getAttribute()))
					{
						from  = fetch;
						found = true;
						break;
					}
				}
				if(!found)
				{
					if(attribute instanceof PluralAttribute)
					{
						from = from.fetch((PluralAttribute)attribute, JoinType.LEFT);
					}
					else
					{
						from = from.fetch((SingularAttribute)attribute, JoinType.LEFT);
					}
				}
			}
		}
	}

	private List<Order> buildJpaOrders(
		final Iterable<OrderBy> orders,
		final Root<E> root,
		final CriteriaBuilder builder)
	{
		final List<Order> jpaOrders = new ArrayList<>();
		for(final OrderBy ob : orders)
		{
			final Path<?> path = Jpa.resolvePath(root, ob.getAttributes());
			jpaOrders.add(ob.isOrderDesc() ? builder.desc(path) : builder.asc(path));
		}
		return jpaOrders;
	}

	private void applyCacheHints(final TypedQuery<?> typedQuery)
	{
		if(this.searchParameters.isCacheable())
		{
			typedQuery.setHint(AvailableHints.HINT_CACHEABLE, true);

			if(this.searchParameters.hasCacheRegion())
			{
				typedQuery.setHint(AvailableHints.HINT_CACHE_REGION, this.searchParameters.getCacheRegion());
			}
			else
			{
				typedQuery.setHint(AvailableHints.HINT_CACHE_REGION, this.persistentClass.getCanonicalName());
			}

			if(this.searchParameters.hasCacheStoreMode())
			{
				typedQuery.setHint(AvailableSettings.JAKARTA_SHARED_CACHE_STORE_MODE,
					this.searchParameters.getCacheStoreMode());
			}

			if(this.searchParameters.hasCacheRetrieveMode())
			{
				typedQuery.setHint(AvailableSettings.JAKARTA_SHARED_CACHE_RETRIEVE_MODE,
					this.searchParameters.getCacheRetrieveMode());
			}
		}
	}

	private void applyPagination(final Query query)
	{
		if(this.searchParameters.getFirst() > 0)
		{
			query.setFirstResult(this.searchParameters.getFirst());
		}
		if(this.searchParameters.getPageSize() > 0)
		{
			query.setMaxResults(this.searchParameters.getPageSize());
		}
		else if(this.searchParameters.getMaxResults() > 0)
		{
			query.setMaxResults(this.searchParameters.getMaxResults());
		}
	}
}

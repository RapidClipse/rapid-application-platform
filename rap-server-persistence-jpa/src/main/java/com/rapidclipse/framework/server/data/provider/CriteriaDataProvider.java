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
package com.rapidclipse.framework.server.data.provider;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import com.rapidclipse.framework.server.data.filter.CriteriaFilterConverter;
import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.jpa.Jpa;
import com.vaadin.flow.data.provider.CallbackDataProvider.CountCallback;
import com.vaadin.flow.data.provider.CallbackDataProvider.FetchCallback;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProviderWrapper;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.provider.Query;
import com.vaadin.flow.data.provider.QuerySortOrder;
import com.vaadin.flow.data.provider.SortDirection;


/**
 * @author XDEV Software
 *
 */
public interface CriteriaDataProvider<T> extends FilterableDataProvider<T, Filter>
{
	public static <T> CriteriaDataProvider<T> New(final CriteriaQuery<T> criteria)
	{
		return New(criteria, CriteriaParameterProvider.Empty());
	}

	public static <T> CriteriaDataProvider<T>
		New(final CriteriaQuery<T> criteria, final CriteriaParameterProvider parameterProvider)
	{
		return new Default<>(criteria, parameterProvider);
	}

	public static class Default<T>
		extends ConfigurableFilterDataProviderWrapper<T, Filter, Filter, Filter>
		implements CriteriaDataProvider<T>
	{
		protected Default(final CriteriaQuery<T> criteria, final CriteriaParameterProvider parameters)
		{
			super(DataProvider.fromFilteringCallbacks(new CriteriaFetchCallback<>(criteria, parameters),
				new CriteriaCountCallback<>(criteria, parameters)));
		}

		@Override
		protected Filter combineFilters(final Filter queryFilter, final Filter configuredFilter)
		{
			if(queryFilter != null && configuredFilter != null)
			{
				return Filter.And(queryFilter, configuredFilter);
			}

			return queryFilter != null ? queryFilter : configuredFilter;
		}

		protected static abstract class CriteriaCallbackBase<T> implements Serializable
		{
			private final CriteriaQuery<T>          criteria;
			private final CriteriaParameterProvider parameterProvider;
			private final Root<?>                   root;

			protected CriteriaCallbackBase(
				final CriteriaQuery<T> criteria,
				final CriteriaParameterProvider parameterProvider)
			{
				super();

				this.criteria          = requireNonNull(criteria);
				this.parameterProvider = requireNonNull(parameterProvider);
				this.root              = Jpa.getRoot(criteria);
				if(this.root == null)
				{
					throw new IllegalArgumentException("Unsupported criteria");
				}
			}

			protected CriteriaQuery<T> criteria()
			{
				return this.criteria;
			}

			protected CriteriaParameterProvider parameterProvider()
			{
				return this.parameterProvider;
			}

			protected Root<?> root()
			{
				return this.root;
			}

			protected EntityManager entityManager()
			{
				return Jpa.getEntityManager(root().getJavaType());
			}

			protected CriteriaQuery<T> createCriteria(
				final Query<T, Filter> query,
				final EntityManager entityManager)
			{
				final CriteriaQuery<T> originalCriteria = criteria();
				final Root<?>          root             = root();

				final CriteriaQuery<T> criteria = entityManager.getCriteriaBuilder()
					.createQuery(originalCriteria.getResultType());
				Jpa.copyCriteria(originalCriteria, criteria);

				final Optional<Filter> optFilter = query.getFilter();
				if(optFilter.isPresent())
				{
					final Filter    filter              = optFilter.get();
					Predicate       predicate           = new CriteriaFilterConverter<>(criteria).apply(filter);
					final Predicate originalWhereClause = originalCriteria.getRestriction();
					if(originalWhereClause != null)
					{
						predicate = entityManager.getCriteriaBuilder().and(predicate, originalWhereClause);
					}
					criteria.where(predicate);
				}

				final List<QuerySortOrder> sortOrders = query.getSortOrders();
				if(sortOrders != null && !sortOrders.isEmpty())
				{
					final List<Order> orders = new ArrayList<>();
					for(final QuerySortOrder sortOrder : sortOrders)
					{
						final Path<?> path = Jpa.resolvePath(root, sortOrder.getSorted());
						if(path != null)
						{
							if(sortOrder.getDirection() == SortDirection.ASCENDING)
							{
								orders.add(entityManager.getCriteriaBuilder().asc(path));
							}
							else
							{
								orders.add(entityManager.getCriteriaBuilder().desc(path));
							}
						}
					}
					orders.addAll(originalCriteria.getOrderList());
					criteria.orderBy(orders);
				}

				return criteria;
			}
		}

		protected static class CriteriaFetchCallback<T> extends CriteriaCallbackBase<T>
			implements FetchCallback<T, Filter>
		{
			protected CriteriaFetchCallback(
				final CriteriaQuery<T> criteria,
				final CriteriaParameterProvider parameterProvider)
			{
				super(criteria, parameterProvider);
			}

			@Override
			public Stream<T> fetch(final Query<T, Filter> query)
			{
				final EntityManager entityManager = entityManager();

				final CriteriaQuery<T> criteria = createCriteria(query, entityManager);

				final TypedQuery<T> typedQuery = entityManager.createQuery(criteria);
				parameterProvider().setParameters(typedQuery);
				typedQuery.setFirstResult(query.getOffset());
				typedQuery.setMaxResults(query.getLimit());

				return typedQuery.getResultList().stream();
			}
		}

		protected static class CriteriaCountCallback<T> extends CriteriaCallbackBase<T>
			implements CountCallback<T, Filter>
		{
			protected CriteriaCountCallback(
				final CriteriaQuery<T> criteria,
				final CriteriaParameterProvider parameterProvider)
			{
				super(criteria, parameterProvider);
			}

			@Override
			public int count(final Query<T, Filter> query)
			{
				final EntityManager       entityManager = entityManager();
				final CriteriaQuery<T>    criteria      = createCriteria(query, entityManager);
				final CriteriaQuery<Long> countCriteria = Jpa.countCriteria(criteria, entityManager);
				final TypedQuery<Long>    typedQuery    = entityManager.createQuery(countCriteria);
				parameterProvider().setParameters(typedQuery);
				return typedQuery.getSingleResult().intValue();
			}
		}
	}
}

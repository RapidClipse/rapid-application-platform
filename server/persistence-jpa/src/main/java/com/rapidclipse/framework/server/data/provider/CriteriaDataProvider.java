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

package com.rapidclipse.framework.server.data.provider;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import com.rapidclipse.framework.server.data.filter.CriteriaFilterConverter;
import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.persistence.jpa.Jpa;
import com.vaadin.flow.data.provider.CallbackDataProvider.CountCallback;
import com.vaadin.flow.data.provider.CallbackDataProvider.FetchCallback;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProviderWrapper;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.provider.Query;
import com.vaadin.flow.data.provider.QuerySortOrder;
import com.vaadin.flow.data.provider.SortDirection;


/**
 * @author XDEV Software
 *
 */
public interface CriteriaDataProvider<T> extends ConfigurableFilterDataProvider<T, Filter, Filter>
{
	public static <T> CriteriaDataProvider<T> New(final CriteriaQuery<T> criteria)
	{
		return new Implementation<>(criteria);
	}
	
	public static class Implementation<T>
		extends ConfigurableFilterDataProviderWrapper<T, Filter, Filter, Filter>
		implements CriteriaDataProvider<T>
	{
		public Implementation(final CriteriaQuery<T> criteria)
		{
			super(DataProvider.fromFilteringCallbacks(new CriteriaFetchCallback<>(criteria),
				new CriteriaCountCallback<>(criteria)));
		}
		
		@Override
		protected Filter combineFilters(final Filter queryFilter, final Filter configuredFilter)
		{
			if(queryFilter != null && configuredFilter != null)
			{
				return Filter.And(queryFilter, configuredFilter);
			}
			
			return configuredFilter;
		}
		
		protected static abstract class CriteriaCallbackBase<T> implements Serializable
		{
			private final CriteriaQuery<T> criteria;
			private final Root<T>          root;
			
			public CriteriaCallbackBase(final CriteriaQuery<T> criteria)
			{
				super();
				
				this.criteria = criteria;
				this.root     = Jpa.findRoot(criteria, criteria.getResultType());
				if(this.root == null)
				{
					throw new IllegalArgumentException("Unsupported criteria");
				}
			}
			
			public CriteriaQuery<T> criteria()
			{
				return this.criteria;
			}
			
			public Root<T> root()
			{
				return this.root;
			}
			
			protected EntityManager entityManager()
			{
				return Jpa.getEntityManager(criteria().getResultType());
			}
			
			protected CriteriaQuery<T> createCriteria(
				final Query<T, Filter> query,
				final EntityManager entityManager)
			{
				final CriteriaQuery<T> originalCriteria = criteria();
				final Root<T>          root             = root();
				
				final CriteriaQuery<T> criteria         = entityManager.getCriteriaBuilder()
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
						predicate = entityManager.getCriteriaBuilder().and(predicate,
							originalWhereClause);
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
			public CriteriaFetchCallback(final CriteriaQuery<T> criteria)
			{
				super(criteria);
			}
			
			@Override
			public Stream<T> fetch(final Query<T, Filter> query)
			{
				final EntityManager    entityManager = entityManager();
				
				final CriteriaQuery<T> criteria      = createCriteria(query, entityManager);
				
				final TypedQuery<T>    typedQuery    = entityManager.createQuery(criteria);
				typedQuery.setFirstResult(query.getOffset());
				typedQuery.setMaxResults(query.getLimit());
				
				return typedQuery.getResultList().stream();
			}
		}
		
		protected static class CriteriaCountCallback<T> extends CriteriaCallbackBase<T>
			implements CountCallback<T, Filter>
		{
			public CriteriaCountCallback(final CriteriaQuery<T> criteria)
			{
				super(criteria);
			}
			
			@Override
			public int count(final Query<T, Filter> query)
			{
				final EntityManager    entityManager = entityManager();
				
				final CriteriaQuery<T> criteria      = createCriteria(query, entityManager);
				
				return Jpa.count(criteria, entityManager).intValue();
			}
		}
	}
}

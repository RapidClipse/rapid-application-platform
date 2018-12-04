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

package software.xdev.rap.server.data.provider;


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

import com.vaadin.flow.data.provider.CallbackDataProvider.CountCallback;
import com.vaadin.flow.data.provider.CallbackDataProvider.FetchCallback;
import com.vaadin.flow.data.provider.Query;
import com.vaadin.flow.data.provider.QuerySortOrder;
import com.vaadin.flow.data.provider.SortDirection;

import software.xdev.rap.server.data.filter.CriteriaFilterConverter;
import software.xdev.rap.server.data.filter.Filter;
import software.xdev.rap.server.persistence.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public class CriteriaDataProviderCallback<T>
		implements FetchCallback<T, Filter>, CountCallback<T, Filter>, Serializable
{
	private final CriteriaQuery<T>	criteria;
	private final Root<T>			root;
	
	
	public CriteriaDataProviderCallback(final CriteriaQuery<T> criteria)
	{
		super();
		
		this.criteria = criteria;
		this.root = Jpa.findRoot(criteria,criteria.getResultType());
		if(this.root == null)
		{
			throw new IllegalArgumentException("Unsupported criteria");
		}
	}
	
	
	@Override
	public Stream<T> fetch(final Query<T, Filter> query)
	{
		final EntityManager entityManager = Jpa.getEntityManager(this.criteria.getResultType());

		final CriteriaQuery<T> criteria = createCriteria(query,entityManager);

		final TypedQuery<T> typedQuery = entityManager.createQuery(criteria);
		typedQuery.setFirstResult(query.getOffset());
		typedQuery.setMaxResults(query.getLimit());

		return typedQuery.getResultList().stream();
	}
	
	
	@Override
	public int count(final Query<T, Filter> query)
	{
		final EntityManager entityManager = Jpa.getEntityManager(this.criteria.getResultType());

		final CriteriaQuery<T> criteria = createCriteria(query,entityManager);
		
		return Jpa.count(criteria,entityManager).intValue();
	}
	
	
	protected CriteriaQuery<T> getCriteria()
	{
		return this.criteria;
	}
	
	
	protected Root<T> getRoot()
	{
		return this.root;
	}
	
	
	protected CriteriaQuery<T> createCriteria(final Query<T, Filter> query,
			final EntityManager entityManager)
	{
		final CriteriaQuery<T> originalCriteria = getCriteria();
		final Root<T> root = getRoot();
		
		final CriteriaQuery<T> criteria = entityManager.getCriteriaBuilder()
				.createQuery(originalCriteria.getResultType());
		Jpa.copyCriteria(originalCriteria,criteria);

		final Optional<Filter> optFilter = query.getFilter();
		if(optFilter.isPresent())
		{
			final Filter filter = optFilter.get();
			Predicate predicate = new CriteriaFilterConverter<>(criteria).convert(filter);
			final Predicate originalWhereClause = originalCriteria.getRestriction();
			if(originalWhereClause != null)
			{
				predicate = entityManager.getCriteriaBuilder().and(predicate,originalWhereClause);
			}
			criteria.where(predicate);
		}

		final List<QuerySortOrder> sortOrders = query.getSortOrders();
		if(sortOrders != null && !sortOrders.isEmpty())
		{
			final List<Order> orders = new ArrayList<>();
			for(final QuerySortOrder sortOrder : sortOrders)
			{
				final Path<?> path = Jpa.getPath(root,sortOrder.getSorted());
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

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa.dal;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import com.rapidclipse.framework.server.data.filter.CriteriaFilterConverter;
import com.rapidclipse.framework.server.data.filter.Filter;


/**
 * @author XDEV Software
 *
 */
public interface PredicateSupplier
{
	public <T> Predicate getPredicate(
		CriteriaQuery<?> criteriaQuery,
		Root<T> root,
		CriteriaBuilder builder,
		T entity);
	
	public static <T> PredicateSupplier New(final Filter filter)
	{
		return new OfFilter(filter);
	}
	
	public static class OfFilter implements PredicateSupplier
	{
		private final Filter filter;
		
		public OfFilter(final Filter filter)
		{
			super();
			this.filter = filter;
		}
		
		@Override
		public <T> Predicate
			getPredicate(
				final CriteriaQuery<?> criteriaQuery,
				final Root<T> root,
				final CriteriaBuilder builder,
				final T entity)
		{
			return new CriteriaFilterConverter<>(criteriaQuery).apply(filter);
		}
	}
}

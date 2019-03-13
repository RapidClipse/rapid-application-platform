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

package com.rapidclipse.framework.server.jpa.dal;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

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

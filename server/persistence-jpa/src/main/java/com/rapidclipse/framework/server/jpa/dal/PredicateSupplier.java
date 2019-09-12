/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
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

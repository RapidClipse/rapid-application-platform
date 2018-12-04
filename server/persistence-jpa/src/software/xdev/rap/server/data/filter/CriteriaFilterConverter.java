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

package software.xdev.rap.server.data.filter;


import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.Attribute;

import org.hibernate.mapping.Collection;

import software.xdev.rap.server.data.filter.Comparison.Equals;
import software.xdev.rap.server.data.filter.Comparison.Greater;
import software.xdev.rap.server.data.filter.Comparison.GreaterEquals;
import software.xdev.rap.server.data.filter.Comparison.Less;
import software.xdev.rap.server.data.filter.Comparison.LessEquals;
import software.xdev.rap.server.data.filter.Comparison.StringComparison;
import software.xdev.rap.server.data.filter.Connector.And;
import software.xdev.rap.server.data.filter.Connector.Or;
import software.xdev.rap.server.persistence.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
@SuppressWarnings("rawtypes")
public class CriteriaFilterConverter<T> implements FilterConverter<Predicate>
{
	private final static char		CRITERIA_WILDCARD	= '%';

	private final CriteriaQuery<T>	criteria;
	private final Root<T>			root;


	public CriteriaFilterConverter(final CriteriaQuery<T> criteria)
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
	public Predicate convert(final Filter filter)
	{
		return convert(filter,Jpa.getEntityManager(this.criteria.getResultType()));
	}


	@SuppressWarnings("unchecked")
	private Predicate convert(final Filter filter, final EntityManager entityManager)
	{
		final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

		if(filter instanceof Connector)
		{
			final Connector connector = (Connector)filter;
			final Predicate[] predicates = connector.filters().stream()
					.map(f -> convert(f,entityManager)).toArray(Predicate[]::new);
			if(connector instanceof And)
			{
				return criteriaBuilder.and(predicates);
			}
			if(connector instanceof Or)
			{
				return criteriaBuilder.or(predicates);
			}
		}

		if(filter instanceof Comparison)
		{
			final Comparison comparison = (Comparison)filter;
			final Path identifier = getPath(comparison.identifier());
			final Comparable value = comparison.value();

			if(filter instanceof Equals)
			{
				if(Collection.class.isAssignableFrom(identifier.getJavaType()))
				{
					return criteriaBuilder.isMember(value,identifier);
				}
				else if(value == null)
				{
					return criteriaBuilder.isNull(identifier);
				}
				else if("".equals(value))
				{
					return criteriaBuilder
							.equal(criteriaBuilder.length(criteriaBuilder.trim(identifier)),0);
				}
				else
				{
					return criteriaBuilder.equal(identifier,value);
				}
			}
			if(filter instanceof Greater)
			{
				return criteriaBuilder.greaterThan(identifier,value);
			}
			if(filter instanceof GreaterEquals)
			{
				return criteriaBuilder.greaterThanOrEqualTo(identifier,value);
			}
			if(filter instanceof Less)
			{
				return criteriaBuilder.lessThan(identifier,value);
			}
			if(filter instanceof LessEquals)
			{
				return criteriaBuilder.lessThanOrEqualTo(identifier,value);
			}
			if(filter instanceof StringComparison)
			{
				final StringComparison stringComparison = (StringComparison)filter;
				String pattern = stringComparison.value();
				for(final char wildcard : stringComparison.wildcards())
				{
					if(wildcard != CRITERIA_WILDCARD)
					{
						pattern = pattern.replace(wildcard,CRITERIA_WILDCARD);
					}
				}
				if(stringComparison.caseSensitive())
				{
					return criteriaBuilder.like(identifier,pattern);
				}
				else
				{
					return criteriaBuilder.like(criteriaBuilder.upper(identifier),
							pattern.toUpperCase());
				}
			}
		}

		if(filter instanceof Between)
		{
			final Between between = (Between)filter;
			final Path identifier = getPath(between.identifier());
			final Comparable start = between.start();
			final Comparable end = between.end();
			return criteriaBuilder.between(identifier,start,end);
		}

		if(filter instanceof IsNull)
		{
			final IsNull isNull = (IsNull)filter;
			final Path identifier = getPath(isNull.identifier());
			return criteriaBuilder.isNull(identifier);
		}

		if(filter instanceof Not)
		{
			final Not not = (Not)filter;
			return criteriaBuilder.not(convert(not.filter(),entityManager));
		}

		throw new IllegalArgumentException(filter.toString());
	}


	private Path<?> getPath(final Object identifier)
	{
		Path<?> path = null;
		if(path instanceof Attribute)
		{
			path = Jpa.getPath(this.root,(Attribute)identifier);
		}
		else
		{
			path = Jpa.getPath(path,identifier.toString());
		}

		if(path == null)
		{
			throw new IllegalArgumentException("Path not found for: " + identifier.toString());
		}

		return path;
	}
}

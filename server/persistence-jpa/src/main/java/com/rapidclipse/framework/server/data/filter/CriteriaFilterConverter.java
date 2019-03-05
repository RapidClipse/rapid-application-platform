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

package com.rapidclipse.framework.server.data.filter;

import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.Attribute;

import org.hibernate.mapping.Collection;

import com.rapidclipse.framework.server.data.filter.Comparison.Equals;
import com.rapidclipse.framework.server.data.filter.Comparison.Greater;
import com.rapidclipse.framework.server.data.filter.Comparison.GreaterEquals;
import com.rapidclipse.framework.server.data.filter.Comparison.Less;
import com.rapidclipse.framework.server.data.filter.Comparison.LessEquals;
import com.rapidclipse.framework.server.data.filter.Comparison.SizeComparison;
import com.rapidclipse.framework.server.data.filter.Comparison.StringComparison;
import com.rapidclipse.framework.server.data.filter.Composite.Connector;
import com.rapidclipse.framework.server.jpa.AttributeChain;
import com.rapidclipse.framework.server.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
@SuppressWarnings("rawtypes")
public class CriteriaFilterConverter<T> implements FilterConverter<Predicate>
{
	private final static char CRITERIA_WILDCARD = '%';
	
	private final CriteriaQuery<T> criteria;
	private final Root<T>          root;
	
	public CriteriaFilterConverter(final CriteriaQuery<T> criteria)
	{
		super();
		
		this.criteria = criteria;
		
		this.root = Jpa.findRoot(criteria, criteria.getResultType());
		if(this.root == null)
		{
			throw new IllegalArgumentException("Unsupported criteria");
		}
	}
	
	@Override
	public Predicate apply(final Filter filter)
	{
		if(filter == null)
		{
			return null;
		}
		
		return convert(filter, Jpa.getEntityManager(this.criteria.getResultType()));
	}
	
	@SuppressWarnings("unchecked")
	private Predicate convert(final Filter filter, final EntityManager entityManager)
	{
		final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		
		if(filter instanceof Composite)
		{
			final Composite   composite  = (Composite)filter;
			final Predicate[] predicates = composite.filters().stream()
				.map(f -> convert(f, entityManager)).toArray(Predicate[]::new);
			if(composite.connector() == Connector.AND)
			{
				return criteriaBuilder.and(predicates);
			}
			if(composite.connector() == Connector.OR)
			{
				return criteriaBuilder.or(predicates);
			}
		}
		
		if(filter instanceof SizeComparison)
		{
			final SizeComparison comparison = (SizeComparison)filter;
			final Path           identifier = getPath(comparison.identifier());
			final Comparable     value      = comparison.value();
			
			if(filter instanceof Greater)
			{
				return criteriaBuilder.greaterThan(identifier, value);
			}
			if(filter instanceof GreaterEquals)
			{
				return criteriaBuilder.greaterThanOrEqualTo(identifier, value);
			}
			if(filter instanceof Less)
			{
				return criteriaBuilder.lessThan(identifier, value);
			}
			if(filter instanceof LessEquals)
			{
				return criteriaBuilder.lessThanOrEqualTo(identifier, value);
			}
		}
		
		if(filter instanceof Comparison)
		{
			final Comparison comparison = (Comparison)filter;
			final Path       identifier = getPath(comparison.identifier());
			final Object     value      = comparison.value();
			
			if(filter instanceof Equals)
			{
				if(Collection.class.isAssignableFrom(identifier.getJavaType()))
				{
					return criteriaBuilder.isMember(value, identifier);
				}
				else if(value == null)
				{
					return criteriaBuilder.isNull(identifier);
				}
				else if("".equals(value))
				{
					return criteriaBuilder
						.equal(criteriaBuilder.length(criteriaBuilder.trim(identifier)), 0);
				}
				else
				{
					return criteriaBuilder.equal(identifier, value);
				}
			}
			
			if(filter instanceof StringComparison)
			{
				final StringComparison stringComparison = (StringComparison)filter;
				String                 pattern          = stringComparison.value();
				for(final char wildcard : stringComparison.wildcards())
				{
					if(wildcard != CRITERIA_WILDCARD)
					{
						pattern = pattern.replace(wildcard, CRITERIA_WILDCARD);
					}
				}
				if(stringComparison.caseSensitive())
				{
					return criteriaBuilder.like(identifier, pattern);
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
			final Between    between    = (Between)filter;
			final Path       identifier = getPath(between.identifier());
			final Comparable start      = between.start();
			final Comparable end        = between.end();
			return criteriaBuilder.between(identifier, start, end);
		}
		
		if(filter instanceof IsNull)
		{
			final IsNull isNull     = (IsNull)filter;
			final Path   identifier = getPath(isNull.identifier());
			return criteriaBuilder.isNull(identifier);
		}
		
		if(filter instanceof Not)
		{
			final Not not = (Not)filter;
			return criteriaBuilder.not(convert(not.filter(), entityManager));
		}
		
		throw new IllegalArgumentException(filter.toString());
	}
	
	private Path<?> getPath(final Object identifier)
	{
		Path<?> path = null;
		if(identifier instanceof Attribute)
		{
			path = Jpa.resolvePath(this.root, (Attribute)identifier);
		}
		else if(identifier instanceof Attribute[])
		{
			path = Jpa.resolvePath(this.root, (Attribute[])identifier);
		}
		else if(identifier instanceof AttributeChain)
		{
			path = Jpa.resolvePath(this.root, ((AttributeChain)identifier).attributes());
		}
		else
		{
			path = Jpa.resolvePath(this.root, identifier.toString());
		}
		
		if(path == null)
		{
			throw new IllegalArgumentException("Path not found for: " + identifier.toString());
		}
		
		return path;
	}
}

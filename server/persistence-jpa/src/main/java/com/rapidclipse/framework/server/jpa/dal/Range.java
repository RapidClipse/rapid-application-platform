/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.jpa.dal;

import java.io.Serializable;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.jpa.AttributeChain;


/**
 * Range support for {@link Comparable} types.
 *
 * @author XDEV Software
 */
@SuppressWarnings("rawtypes")
public interface Range<E, D extends Comparable> extends Serializable
{
	/**
	 * @return the entity's attribute this Range refers to.
	 */
	public AttributeChain<?, ?> getAttributes();
	
	/**
	 * @return the lower range boundary or null for unbound lower range.
	 */
	public D getFrom();
	
	/**
	 * Sets the lower range boundary. Accepts null for unbound lower range.
	 */
	public void setFrom(final D from);
	
	public default Range<E, D> from(final D from)
	{
		setFrom(from);
		return this;
	}
	
	public default boolean isFromSet()
	{
		return getFrom() != null;
	}
	
	/**
	 * @return the upper range boundary or null for unbound upper range.
	 */
	public D getTo();
	
	/**
	 * Sets the upper range boundary. Accepts null for unbound upper range.
	 */
	public void setTo(final D to);
	
	public default Range<E, D> to(final D to)
	{
		setTo(to);
		return this;
	}
	
	public default boolean isToSet()
	{
		return getTo() != null;
	}
	
	public void setIncludeNull(final Boolean includeNull);
	
	public Boolean getIncludeNull();
	
	public default Range<E, D> includeNull(final Boolean includeNull)
	{
		setIncludeNull(includeNull);
		return this;
	}
	
	public default boolean isIncludeNullSet()
	{
		return getIncludeNull() != null;
	}
	
	public default boolean isBetween()
	{
		return isFromSet() && isToSet();
	}
	
	public default boolean isSet()
	{
		return isFromSet() || isToSet() || isIncludeNullSet();
	}
	
	@SuppressWarnings("unchecked")
	public default boolean isValid()
	{
		if(isBetween())
		{
			return getFrom().compareTo(getTo()) <= 0;
		}
		return true;
	}
	
	public void reset();
	
	/**
	 * Constructs a new Range with no boundaries and no restrictions on field's
	 * nullability.
	 *
	 * @param attributes
	 *            the path to the attribute of an existing entity.
	 */
	public static <E, D extends Comparable> Range<E, D> New(final Attribute<?, ?>... attributes)
	{
		return new Default<>(attributes);
	}
	
	/**
	 * Constructs a new Range.
	 *
	 * @param from
	 *            the lower boundary of this range. Null means no lower
	 *            boundary.
	 * @param to
	 *            the upper boundary of this range. Null means no upper
	 *            boundary.
	 * @param attributes
	 *            the path to the attribute of an existing entity.
	 */
	public static <E, D extends Comparable> Range<E, D>
		New(final D from, final D to, final Attribute<?, ?>... attributes)
	{
		return new Default<>(from, to, attributes);
	}
	
	/**
	 * Constructs a new Range.
	 *
	 * @param from
	 *            the lower boundary of this range. Null means no lower
	 *            boundary.
	 * @param to
	 *            the upper boundary of this range. Null means no upper
	 *            boundary.
	 * @param includeNull
	 *            tells whether null should be filtered out or not.
	 * @param attributes
	 *            the path to the attribute of an existing entity.
	 */
	public static <E, D extends Comparable> Range<E, D> New(
		final D from,
		final D to,
		final Boolean includeNull,
		final Attribute<?, ?>... attributes)
	{
		return new Default<>(from, to, includeNull, attributes);
	}
	
	public static class Default<E, D extends Comparable> implements Range<E, D>
	{
		private final PathHolder pathHolder;
		private D                from;
		private D                to;
		private Boolean          includeNull;
		
		protected Default(final Attribute<?, ?>... attributes)
		{
			this.pathHolder = PathHolder.New(AttributeChain.New(attributes));
		}
		
		protected Default(final D from, final D to, final Attribute<?, ?>... attributes)
		{
			this(attributes);
			this.from = from;
			this.to   = to;
		}
		
		protected Default(
			final D from,
			final D to,
			final Boolean includeNull,
			final Attribute<?, ?>... attributes)
		{
			this(from, to, attributes);
			this.includeNull = includeNull;
		}
		
		@Override
		public AttributeChain<?, ?> getAttributes()
		{
			return this.pathHolder.getAttributes();
		}
		
		@Override
		public D getFrom()
		{
			return this.from;
		}
		
		@Override
		public void setFrom(final D from)
		{
			this.from = from;
		}
		
		@Override
		public D getTo()
		{
			return this.to;
		}
		
		@Override
		public void setTo(final D to)
		{
			this.to = to;
		}
		
		@Override
		public void setIncludeNull(final Boolean includeNull)
		{
			this.includeNull = includeNull;
		}
		
		@Override
		public Boolean getIncludeNull()
		{
			return this.includeNull;
		}
		
		@Override
		public void reset()
		{
			this.from        = null;
			this.to          = null;
			this.includeNull = null;
		}
	}
}

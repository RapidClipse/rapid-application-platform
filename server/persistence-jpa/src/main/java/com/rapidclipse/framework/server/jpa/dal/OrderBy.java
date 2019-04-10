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

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.jpa.AttributeChain;


/**
 * Holder class for search ordering used by the {@link SearchParameters}.
 *
 * @author XDEV Software
 */
public interface OrderBy extends Serializable
{
	public static enum Direction
	{
		ASC,
		DESC;
	}

	public PathHolder getPathHolder();

	public default AttributeChain<?, ?> getAttributes()
	{
		return getPathHolder().getAttributes();
	}

	public default String getPath()
	{
		return getPathHolder().getPath();
	}

	public Direction getDirection();

	public default boolean isOrderDesc()
	{
		return Direction.DESC == getDirection();
	}

	public static OrderBy New(final Direction direction, final Attribute<?, ?>... attributes)
	{
		return new Implementation(direction, attributes);
	}

	public static OrderBy New(final Direction direction, final String path, final Class<?> from)
	{
		return new Implementation(direction, path, from);
	}

	public static class Implementation implements OrderBy
	{
		private final PathHolder pathHolder;
		private Direction        direction = Direction.ASC;

		public Implementation(final Direction direction, final Attribute<?, ?>... attributes)
		{
			this.direction  = direction;
			this.pathHolder = PathHolder.New(AttributeChain.New(attributes));
		}

		public Implementation(final Direction direction, final String path, final Class<?> from)
		{
			this.direction  = direction;
			this.pathHolder = PathHolder.New(path, from);
		}

		@Override
		public PathHolder getPathHolder()
		{
			return this.pathHolder;
		}

		@Override
		public Direction getDirection()
		{
			return this.direction;
		}

		@Override
		public int hashCode()
		{
			final int prime  = 31;
			int       result = 1;
			result = prime * result + ((this.pathHolder == null) ? 0 : this.pathHolder.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj)
		{
			if(this == obj)
			{
				return true;
			}
			if(obj == null)
			{
				return false;
			}
			if(!(obj instanceof OrderBy))
			{
				return false;
			}
			final OrderBy other = (OrderBy)obj;
			return Objects.equals(this.pathHolder, other.getPathHolder());
		}
	}
}

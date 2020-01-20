/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
		return new Default(direction, attributes);
	}
	
	public static OrderBy New(final Direction direction, final String path, final Class<?> from)
	{
		return new Default(direction, path, from);
	}
	
	public static class Default implements OrderBy
	{
		private final PathHolder pathHolder;
		private Direction        direction = Direction.ASC;
		
		protected Default(final Direction direction, final Attribute<?, ?>... attributes)
		{
			this.direction  = direction;
			this.pathHolder = PathHolder.New(AttributeChain.New(attributes));
		}
		
		protected Default(final Direction direction, final String path, final Class<?> from)
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

/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

import org.hibernate.criterion.Example.PropertySelector;

import com.rapidclipse.framework.server.jpa.AttributeChain;
import com.rapidclipse.framework.server.jpa.Jpa;


/**
 * Holder class for path used by the {@link OrderBy}, {@link PropertySelector},
 * {@link TermSelector} and {@link SearchParameters}.
 *
 * @author XDEV Software
 */
public interface PathHolder extends Serializable
{
	public AttributeChain<?, ?> getAttributes();
	
	public String getPath();

	public static PathHolder New(final AttributeChain<?, ?> attributeChain)
	{
		return new Default(attributeChain);
	}

	public static PathHolder New(final String path, final Class<?> from)
	{
		return new Default(path, from);
	}
	
	public static class Default implements PathHolder
	{
		private final String                   path;
		private final Class<?>                 from;
		private transient AttributeChain<?, ?> attributeChain;

		protected Default(final AttributeChain<?, ?> attributeChain)
		{
			this.attributeChain = attributeChain;
			this.path           = attributeChain.path();
			this.from           = attributeChain.first().getDeclaringType().getJavaType();
		}

		protected Default(final String path, final Class<?> from)
		{
			this.path = path;
			this.from = from;
			if(getAttributes() == null)
			{
				throw new IllegalArgumentException();
			}
		}

		@Override
		public AttributeChain<?, ?> getAttributes()
		{
			if(this.attributeChain == null)
			{
				this.attributeChain = Jpa.resolveAttributeChain(this.from, this.path);
			}
			return this.attributeChain;
		}

		@Override
		public String getPath()
		{
			return this.path;
		}

		@Override
		public int hashCode()
		{
			final int prime  = 31;
			int       result = 1;
			result = prime * result + ((this.path == null) ? 0 : this.path.hashCode());
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
			if(!(obj instanceof PathHolder))
			{
				return false;
			}
			final PathHolder other = (PathHolder)obj;
			return Objects.equals(getPath(), other.getPath());
		}
	}
}

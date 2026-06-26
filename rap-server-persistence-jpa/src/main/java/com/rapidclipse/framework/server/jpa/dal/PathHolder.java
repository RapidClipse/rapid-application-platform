/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa.dal;

import java.io.Serializable;
import java.util.Objects;


import com.rapidclipse.framework.server.jpa.AttributeChain;
import com.rapidclipse.framework.server.jpa.Jpa;


/**
 * Holder class for path used by the {@link OrderBy}, {@link PropertySelector},
 * {@link PathHolder}, {@link Range} and {@link SearchParameters}.
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

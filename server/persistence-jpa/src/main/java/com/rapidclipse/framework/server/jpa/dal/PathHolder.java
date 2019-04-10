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
		return new Implementation(attributeChain);
	}
	
	public static PathHolder New(final String path, final Class<?> from)
	{
		return new Implementation(path, from);
	}

	public static class Implementation implements PathHolder
	{
		private final String                   path;
		private final Class<?>                 from;
		private transient AttributeChain<?, ?> attributeChain;
		
		public Implementation(final AttributeChain<?, ?> attributeChain)
		{
			this.attributeChain = attributeChain;
			this.path           = attributeChain.path();
			this.from           = attributeChain.first().getDeclaringType().getJavaType();
		}
		
		public Implementation(final String path, final Class<?> from)
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

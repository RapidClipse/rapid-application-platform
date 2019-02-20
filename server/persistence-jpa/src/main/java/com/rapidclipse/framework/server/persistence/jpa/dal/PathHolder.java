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

package com.rapidclipse.framework.server.persistence.jpa.dal;

import java.io.Serializable;

import org.hibernate.criterion.Example.PropertySelector;

import com.rapidclipse.framework.server.persistence.jpa.AttributeChain;
import com.rapidclipse.framework.server.persistence.jpa.Jpa;


/**
 * Holder class for path used by the {@link OrderBy}, {@link PropertySelector},
 * {@link TermSelector} and {@link SearchParameters}.
 *
 * @author XDEV Software
 */
public class PathHolder implements Serializable
{
	private static final long        serialVersionUID = 1L;
	private final String             path;
	private final Class<?>           from;
	private transient AttributeChain attributeChain;
	
	public PathHolder(final AttributeChain attributeChain)
	{
		if(!attributeChain.verify())
		{
			throw new IllegalArgumentException("Invalid attribute chain");
		}
		
		this.attributeChain = attributeChain;
		this.path           = attributeChain.path();
		this.from           = attributeChain.first().getDeclaringType().getJavaType();
	}
	
	public PathHolder(final String path, final Class<?> from)
	{
		this.path = path;
		this.from = from;
		if(getAttributes() == null)
		{
			throw new IllegalArgumentException();
		}
	}
	
	public AttributeChain getAttributes()
	{
		if(this.attributeChain == null)
		{
			this.attributeChain = Jpa.resolveAttributeChain(this.from, this.path);
		}
		return this.attributeChain;
	}
	
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
		if(getClass() != obj.getClass())
		{
			return false;
		}
		final PathHolder other = (PathHolder)obj;
		if(this.path == null)
		{
			if(other.path != null)
			{
				return false;
			}
		}
		else if(!this.path.equals(other.path))
		{
			return false;
		}
		return true;
	}
	
}

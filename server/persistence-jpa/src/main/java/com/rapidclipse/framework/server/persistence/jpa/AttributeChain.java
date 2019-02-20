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

package com.rapidclipse.framework.server.persistence.jpa;

import static com.rapidclipse.framework.server.Rap.notEmpty;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.persistence.metamodel.Attribute;
import javax.persistence.metamodel.PluralAttribute;


/**
 * @author XDEV Software
 *
 */
public final class AttributeChain implements Iterable<Attribute<?, ?>>, Cloneable
{
	private final List<Attribute<?, ?>> attributes;
	
	public AttributeChain(final Collection<? extends Attribute<?, ?>> attributes)
	{
		super();
		
		this.attributes = new ArrayList<>(notEmpty(attributes));
	}
	
	public AttributeChain(final Attribute<?, ?>... attributes)
	{
		super();
		
		this.attributes = Arrays.asList(notEmpty(attributes));
	}
	
	public Iterable<Attribute<?, ?>> attributes()
	{
		return this.attributes;
	}
	
	public Attribute<?, ?> first()
	{
		return this.attributes.get(0);
	}
	
	public Attribute<?, ?> last()
	{
		return this.attributes.get(this.attributes.size() - 1);
	}
	
	public String path()
	{
		return Jpa.toPropertyPath(this);
	}
	
	@SuppressWarnings("rawtypes")
	public boolean verify()
	{
		final List<Attribute<?, ?>> attributes = new ArrayList<>(this.attributes);
		Class<?>                    from       = null;
		if(attributes.get(0).isCollection())
		{
			from = ((PluralAttribute)attributes.get(0)).getElementType().getJavaType();
		}
		else
		{
			from = attributes.get(0).getJavaType();
		}
		attributes.remove(0);
		for(final Attribute<?, ?> attribute : attributes)
		{
			if(!attribute.getDeclaringType().getJavaType().isAssignableFrom(from))
			{
				return false;
			}
			from = attribute.getJavaType();
		}
		
		return true;
	}
	
	@Override
	public Iterator<Attribute<?, ?>> iterator()
	{
		return this.attributes.iterator();
	}
	
	@Override
	public AttributeChain clone()
	{
		return new AttributeChain(this.attributes);
	}
	
	@Override
	public String toString()
	{
		return path();
	}
	
	@Override
	public boolean equals(final Object obj)
	{
		return obj == this || (obj instanceof AttributeChain
			&& this.attributes.equals(((AttributeChain)obj).attributes));
	}
	
	@Override
	public int hashCode()
	{
		return this.attributes.hashCode();
	}
}

/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.jpa;

import static com.rapidclipse.framework.server.Rap.notEmpty;

import java.io.Serializable;
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
public interface AttributeChain<X, Y> extends Iterable<Attribute<?, ?>>, Cloneable, Serializable
{
	public Iterable<Attribute<?, ?>> attributes();
	
	public Attribute<X, ?> first();
	
	public Attribute<?, Y> last();
	
	public String path();
	
	public AttributeChain<X, Y> clone();
	
	@Override
	public default Iterator<Attribute<?, ?>> iterator()
	{
		return attributes().iterator();
	}
	
	public static <X, Y> Builder<X, Y> Builder(final Attribute<X, Y> first)
	{
		return new Builder.Default<>(first);
	}
	
	public static interface Builder<X, Y>
	{
		public <A> Builder<X, A> add(Attribute<Y, A> attribute);
		
		public AttributeChain<X, Y> build();
		
		public static class Default<X, Y> implements Builder<X, Y>
		{
			private final List<Attribute<?, ?>> attributes = new ArrayList<>();
			
			protected Default(final Attribute<X, Y> first)
			{
				this.attributes.add(first);
			}
			
			@SuppressWarnings("unchecked")
			@Override
			public <A> Builder<X, A> add(final Attribute<Y, A> attribute)
			{
				this.attributes.add(attribute);
				return (Builder<X, A>)this;
			}
			
			@Override
			public AttributeChain<X, Y> build()
			{
				return new AttributeChain.Default<>(this.attributes);
			}
		}
	}
	
	public static AttributeChain<?, ?> New(final Collection<? extends Attribute<?, ?>> attributes)
	{
		return new Default<>(attributes);
	}
	
	public static <X, Y> AttributeChain<X, Y> New(final Attribute<X, Y> singleAttribute)
	{
		return Builder(singleAttribute).build();
	}
	
	public static <X, T1, Y> AttributeChain<X, Y> New(
		final Attribute<X, T1> attribute1,
		final Attribute<T1, Y> attribute2)
	{
		return Builder(attribute1).add(attribute2).build();
	}
	
	public static <X, T1, T2, Y> AttributeChain<X, Y> New(
		final Attribute<X, T1> attribute1,
		final Attribute<T1, T2> attribute2,
		final Attribute<T2, Y> attribute3)
	{
		return Builder(attribute1).add(attribute2).add(attribute3).build();
	}
	
	public static <X, T1, T2, T3, Y> AttributeChain<X, Y> New(
		final Attribute<X, T1> attribute1,
		final Attribute<T1, T2> attribute2,
		final Attribute<T2, T3> attribute3,
		final Attribute<T3, Y> attribute4)
	{
		return Builder(attribute1).add(attribute2).add(attribute3).add(attribute4).build();
	}
	
	public static <X, T1, T2, T3, T4, Y> AttributeChain<X, Y> New(
		final Attribute<X, T1> attribute1,
		final Attribute<T1, T2> attribute2,
		final Attribute<T2, T3> attribute3,
		final Attribute<T3, T4> attribute4,
		final Attribute<T4, Y> attribute5)
	{
		return Builder(attribute1).add(attribute2).add(attribute3).add(attribute4).add(attribute5).build();
	}
	
	public static AttributeChain<?, ?> New(final Attribute<?, ?>... attributes)
	{
		return new Default<>(attributes);
	}
	
	public static class Default<X, Y> implements AttributeChain<X, Y>
	{
		private final List<Attribute<?, ?>> attributes;
		
		protected Default(final Collection<? extends Attribute<?, ?>> attributes)
		{
			super();
			
			this.attributes = verify(new ArrayList<>(notEmpty(attributes)));
		}
		
		protected Default(final Attribute<?, ?>... attributes)
		{
			super();
			
			this.attributes = verify(Arrays.asList(notEmpty(attributes)));
		}
		
		@SuppressWarnings("rawtypes")
		private static List<Attribute<?, ?>> verify(final List<Attribute<?, ?>> attributes)
		{
			Class<?> from = null;
			if(attributes.get(0).isCollection())
			{
				from = ((PluralAttribute)attributes.get(0)).getElementType().getJavaType();
			}
			else
			{
				from = attributes.get(0).getJavaType();
			}
			
			for(int i = 1, c = attributes.size(); i < c; i++)
			{
				final Attribute<?, ?> attribute = attributes.get(i);
				if(!attribute.getDeclaringType().getJavaType().isAssignableFrom(from))
				{
					throw new IllegalArgumentException("Invalid attribute chain: " +
						attribute.getDeclaringType().getJavaType().getName() + " <> " + from.getName());
				}
				from = attribute.getJavaType();
			}
			
			return attributes;
		}
		
		@Override
		public Iterable<Attribute<?, ?>> attributes()
		{
			return this.attributes;
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Attribute<X, ?> first()
		{
			return (Attribute<X, ?>)this.attributes.get(0);
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Attribute<?, Y> last()
		{
			return (Attribute<?, Y>)this.attributes.get(this.attributes.size() - 1);
		}
		
		@Override
		public String path()
		{
			return Jpa.toPropertyPath(this);
		}
		
		@Override
		public AttributeChain<X, Y> clone()
		{
			return new Default<>(this.attributes);
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
				&& this.attributes.equals(((AttributeChain<?, ?>)obj).attributes()));
		}
		
		@Override
		public int hashCode()
		{
			return this.attributes.hashCode();
		}
	}
}

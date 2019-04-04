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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.jpa.AttributeChain;


/**
 * Used to construct OR predicate for a property value. In other words you can
 * search all entities E having a given property set to one of the selected
 * values.
 *
 * @author XDEV Software
 */
public interface PropertySelector<E, F> extends Serializable
{
	public AttributeChain getAttributes();
	
	public boolean isNotIncludingNullSet();
	
	public Boolean isNotIncludingNull();
	
	public PropertySelector<E, F> withoutNull();
	
	public List<F> getSelected();
	
	public PropertySelector<E, F> add(final F object);
	
	public void setSelected(final List<F> selected);

	@SuppressWarnings("unchecked")
	public default PropertySelector<E, F> selected(final F... selected)
	{
		setSelected(Arrays.asList(selected));
		return this;
	}
	
	public boolean isNotEmpty();
	
	public void clearSelected();
	
	public default void setValue(final F value)
	{
		setSelected(Arrays.asList(value));
	}
	
	public default F getValue()
	{
		return isNotEmpty() ? getSelected().get(0) : null;
	}
	
	public default boolean isBoolean()
	{
		return isType(Boolean.class);
	}
	
	public default boolean isString()
	{
		return isType(String.class);
	}
	
	public default boolean isNumber()
	{
		return isType(Number.class);
	}
	
	public default boolean isType(final Class<?> type)
	{
		return type.isAssignableFrom(getAttributes().last().getJavaType());
	}
	
	public SearchMode getSearchMode();
	
	/**
	 * In case, the field's type is a String, you can set a searchMode to use.
	 * It is null by default.
	 */
	public void setSearchMode(final SearchMode searchMode);
	
	public default PropertySelector<E, F> searchMode(final SearchMode searchMode)
	{
		setSearchMode(searchMode);
		return this;
	}
	
	public boolean isOrMode();
	
	public void setOrMode(final boolean orMode);
	
	public default PropertySelector<E, F> orMode(final boolean orMode)
	{
		setOrMode(orMode);
		return this;
	}
	
	public static <E, F> PropertySelector<E, F> New(final Attribute<?, ?>... fields)
	{
		return new Implementation<E, F>(fields);
	}
	
	public static <E, F> PropertySelector<E, F> New(final String path, final Class<E> from)
	{
		return new Implementation<E, F>(path, from);
	}
	
	public static <E, F> PropertySelector<E, F> New(final boolean orMode, final Attribute<?, ?>... fields)
	{
		return new Implementation<E, F>(fields).orMode(orMode);
	}
	
	public static class Implementation<E, F> implements PropertySelector<E, F>
	{
		private final PathHolder pathHolder;
		private List<F>          selected = new ArrayList<>();
		// for string property only
		private SearchMode searchMode;
		private Boolean    notIncludingNull;
		private boolean    orMode = true;
		
		public Implementation(final Attribute<?, ?>... attributes)
		{
			this.pathHolder = PathHolder.New(AttributeChain.New(attributes));
		}
		
		public Implementation(final String path, final Class<E> from)
		{
			this.pathHolder = PathHolder.New(path, from);
		}
		
		@Override
		public AttributeChain getAttributes()
		{
			return this.pathHolder.getAttributes();
		}
		
		@Override
		public boolean isNotIncludingNullSet()
		{
			return this.notIncludingNull != null;
		}
		
		@Override
		public Boolean isNotIncludingNull()
		{
			return this.notIncludingNull;
		}
		
		@Override
		public PropertySelector<E, F> withoutNull()
		{
			this.notIncludingNull = true;
			return this;
		}
		
		@Override
		public List<F> getSelected()
		{
			return this.selected;
		}
		
		@Override
		public PropertySelector<E, F> add(final F object)
		{
			this.selected.add(object);
			return this;
		}
		
		@Override
		public void setSelected(final List<F> selected)
		{
			this.selected = new ArrayList<>(selected);
		}
		
		@Override
		public boolean isNotEmpty()
		{
			return this.selected != null && !this.selected.isEmpty();
		}
		
		@Override
		public void clearSelected()
		{
			if(this.selected != null)
			{
				this.selected.clear();
			}
		}
		
		@Override
		public SearchMode getSearchMode()
		{
			return this.searchMode;
		}
		
		@Override
		public void setSearchMode(final SearchMode searchMode)
		{
			this.searchMode = searchMode;
		}
		
		@Override
		public boolean isOrMode()
		{
			return this.orMode;
		}
		
		@Override
		public void setOrMode(final boolean orMode)
		{
			this.orMode = orMode;
		}
	}
}

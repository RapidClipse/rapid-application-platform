/*-
 * ---
 * Rapid Application Platform / Server / Persistence / JPA
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.persistence.jpa.dal;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.persistence.jpa.AttributeChain;


/**
 * Used to construct OR predicate for a property value. In other words you can
 * search all entities E having a given property set to one of the selected
 * values.
 *
 * @author XDEV Software
 */
public class PropertySelector<E, F> implements Serializable
{
	/*
	 * PropertySelector builder
	 */
	public static <E, F> PropertySelector<E, F> newPropertySelector(final Attribute<?, ?>... fields)
	{
		return new PropertySelector<E, F>(fields);
	}
	
	/*
	 * PropertySelector builder
	 */
	public static <E, F> PropertySelector<E, F> newPropertySelector(
		final String path,
		final Class<E> from)
	{
		return new PropertySelector<E, F>(path, from);
	}
	
	/*
	 * PropertySelector builder
	 */
	public static <E, F> PropertySelector<E, F> newPropertySelector(
		final boolean orMode,
		final Attribute<?, ?>... fields)
	{
		final PropertySelector<E, F> ps = new PropertySelector<E, F>(fields);
		return ps.orMode(orMode);
	}

	private static final long serialVersionUID = 1L;

	private final PathHolder  pathHolder;
	private List<F>           selected         = new ArrayList<>();
	private SearchMode        searchMode;                          // for
																	// string
																	// property
																	// only.
	private Boolean           notIncludingNull;
	private boolean           orMode           = true;
	
	public PropertySelector(final Attribute<?, ?>... attributes)
	{
		this.pathHolder = new PathHolder(new AttributeChain(attributes));
	}
	
	public PropertySelector(final String path, final Class<E> from)
	{
		this.pathHolder = new PathHolder(path, from);
	}
	
	public AttributeChain getAttributes()
	{
		return this.pathHolder.getAttributes();
	}
	
	public boolean isNotIncludingNullSet()
	{
		return this.notIncludingNull != null;
	}
	
	public Boolean isNotIncludingNull()
	{
		return this.notIncludingNull;
	}
	
	public PropertySelector<E, F> withoutNull()
	{
		this.notIncludingNull = true;
		return this;
	}
	
	/*
	 * Get the possible candidates for property.
	 */
	public List<F> getSelected()
	{
		return this.selected;
	}
	
	public PropertySelector<E, F> add(final F object)
	{
		this.selected.add(object);
		return this;
	}
	
	/*
	 * Set the possible candidates for property.
	 */
	public void setSelected(final List<F> selected)
	{
		this.selected = new ArrayList<>(selected);
	}
	
	@SuppressWarnings("unchecked")
	public PropertySelector<E, F> selected(final F... selected)
	{
		setSelected(Arrays.asList(selected));
		return this;
	}
	
	public boolean isNotEmpty()
	{
		return this.selected != null && !this.selected.isEmpty();
	}
	
	public void clearSelected()
	{
		if(this.selected != null)
		{
			this.selected.clear();
		}
	}
	
	public void setValue(final F value)
	{
		setSelected(Arrays.asList(value));
	}
	
	public F getValue()
	{
		return isNotEmpty() ? this.selected.get(0) : null;
	}
	
	public boolean isBoolean()
	{
		return isType(Boolean.class);
	}
	
	public boolean isString()
	{
		return isType(String.class);
	}
	
	public boolean isNumber()
	{
		return isType(Number.class);
	}
	
	public boolean isType(final Class<?> type)
	{
		return type.isAssignableFrom(getAttributes().last().getJavaType());
	}
	
	public SearchMode getSearchMode()
	{
		return this.searchMode;
	}
	
	/**
	 * In case, the field's type is a String, you can set a searchMode to use.
	 * It is null by default.
	 */
	public void setSearchMode(final SearchMode searchMode)
	{
		this.searchMode = searchMode;
	}
	
	public PropertySelector<E, F> searchMode(final SearchMode searchMode)
	{
		setSearchMode(searchMode);
		return this;
	}
	
	public boolean isOrMode()
	{
		return this.orMode;
	}
	
	public void setOrMode(final boolean orMode)
	{
		this.orMode = orMode;
	}
	
	public PropertySelector<E, F> orMode(final boolean orMode)
	{
		setOrMode(orMode);
		return this;
	}
}

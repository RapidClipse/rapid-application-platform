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

package com.rapidclipse.framework.server.ui.filter;

import static java.util.stream.Collectors.toList;

import java.util.List;
import java.util.Optional;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.rapidclipse.framework.server.util.ReflectionUtils;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.data.binder.BeanPropertySet;
import com.vaadin.flow.data.binder.PropertyDefinition;


/**
 * @author XDEV Software
 *
 */
public class GridFilterSubjectFactory implements FilterSubjectFactory
{
	@Override
	public boolean supports(final Object source)
	{
		return source instanceof Grid && getBeanPropertySet((Grid<?>)source) != null;
	}
	
	@Override
	public FilterSubject createFilterSubject(final Object source)
	{
		final Grid<?>            grid        = (Grid<?>)source;
		final BeanPropertySet<?> propertySet = getBeanPropertySet(grid);
		final Class<?>           beanType    = propertySet.getBeanType();
		
		final List<FilterProperty<?>> searchableProperties = grid.getColumns().stream()
			.map(c -> propertySet.getProperty(c.getKey())).filter(Optional::isPresent)
			.map(Optional::get).filter(p -> String.class.equals(p.getType()))
			.map(d -> toFilterProperty(beanType, d)).collect(toList());
		
		final List<FilterProperty<?>> filterableProperties = grid.getColumns().stream()
			.map(c -> propertySet.getProperty(c.getKey())).filter(Optional::isPresent)
			.map(Optional::get).filter(p -> Comparable.class.isAssignableFrom(p.getType()))
			.map(d -> toFilterProperty(beanType, d)).collect(toList());
		
		return FilterSubject.New(searchableProperties, filterableProperties);
	}
	
	protected FilterProperty<?> toFilterProperty(
		final Class<?> clazz,
		final PropertyDefinition<?, ?> propertyDefinition)
	{
		final String name = propertyDefinition.getName();
		return FilterProperty.New(name, propertyDefinition.getType(),
			CaptionUtils.resolveCaption(clazz, name));
	}
	
	protected BeanPropertySet<?> getBeanPropertySet(final Grid<?> grid)
	{
		try
		{
			final Object o = ReflectionUtils.getMemberValue(grid,
				Grid.class.getDeclaredField("propertySet"));
			if(o instanceof BeanPropertySet<?>)
			{
				return (BeanPropertySet<?>)o;
			}
		}
		catch(NoSuchFieldException | SecurityException e)
		{
		}
		
		return null;
	}
}

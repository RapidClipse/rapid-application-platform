/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
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
		final Grid<?> grid = (Grid<?>)source;
		final BeanPropertySet<?> propertySet = getBeanPropertySet(grid);
		final Class<?> beanType = propertySet.getBeanType();

		final List<FilterProperty<?>> searchableProperties = grid.getColumns().stream()
				.map(c -> propertySet.getProperty(c.getKey())).filter(Optional::isPresent)
				.map(Optional::get).filter(p -> String.class.equals(p.getType()))
				.map(d -> toFilterProperty(beanType,d)).collect(toList());

		final List<FilterProperty<?>> filterableProperties = grid.getColumns().stream()
				.map(c -> propertySet.getProperty(c.getKey())).filter(Optional::isPresent)
				.map(Optional::get).filter(p -> Comparable.class.isAssignableFrom(p.getType()))
				.map(d -> toFilterProperty(beanType,d)).collect(toList());

		return FilterSubject.New(searchableProperties,filterableProperties);
	}
	
	
	protected FilterProperty<?> toFilterProperty(final Class<?> clazz,
			final PropertyDefinition<?, ?> propertyDefinition)
	{
		final String name = propertyDefinition.getName();
		return FilterProperty.New(name,propertyDefinition.getType(),
				CaptionUtils.resolveCaption(clazz,name));
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

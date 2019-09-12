/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.filter;

import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toList;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.data.binder.BeanPropertySet;
import com.vaadin.flow.data.binder.PropertyDefinition;
import com.vaadin.flow.data.binder.PropertySet;


/**
 * @author XDEV Software
 *
 */
public class GridFilterSubjectFactory extends FilterSubjectFactory.Abstract<Grid<?>>
{
	public static GridFilterSubjectFactory New()
	{
		return new GridFilterSubjectFactory();
	}
	
	public static GridFilterSubjectFactory New(
		final Predicate<PropertyDefinition<?, ?>> searchablePropertyFilter,
		final Predicate<PropertyDefinition<?, ?>> filterablePropertyFilter)
	{
		return new GridFilterSubjectFactory(searchablePropertyFilter, filterablePropertyFilter);
	}
	
	public static GridFilterSubjectFactory New(
		final Collection<String> searchableProperties,
		final Collection<String> filterableProperties)
	{
		return new GridFilterSubjectFactory(
			d -> searchableProperties.contains(d.getName()),
			d -> filterableProperties.contains(d.getName()));
	}
	
	public static FilterSubject CreateFilterSubject(final Grid<?> grid)
	{
		return New().createFilterSubject(grid);
	}

	public static FilterSubject CreateFilterSubject(
		final Grid<?> grid,
		final Collection<String> searchableProperties,
		final Collection<String> filterableProperties)
	{
		return New(searchableProperties, filterableProperties).createFilterSubject(grid);
	}

	private final Predicate<PropertyDefinition<?, ?>> searchablePropertyFilter;
	private final Predicate<PropertyDefinition<?, ?>> filterablePropertyFilter;
	
	public GridFilterSubjectFactory()
	{
		this(d -> true, d -> true);
	}
	
	public GridFilterSubjectFactory(
		final Predicate<PropertyDefinition<?, ?>> searchablePropertyFilter,
		final Predicate<PropertyDefinition<?, ?>> filterablePropertyFilter)
	{
		super();
		
		this.searchablePropertyFilter = requireNonNull(searchablePropertyFilter);
		this.filterablePropertyFilter = requireNonNull(filterablePropertyFilter);
	}

	@Override
	public FilterSubject createFilterSubject(final Grid<?> grid)
	{
		final PropertySet<?> propertySet = grid.getPropertySet();
		if(!(propertySet instanceof BeanPropertySet))
		{
			throw new IllegalArgumentException("Grid has no BeanPropertySet");
		}

		final Class<?> beanType = ((BeanPropertySet<?>)propertySet).getBeanType();

		final List<FilterProperty<?>> searchableProperties = grid.getColumns().stream()
			.map(c -> propertySet.getProperty(c.getKey()))
			.filter(Optional::isPresent).map(Optional::get)
			.filter(p -> isSearchable(p.getType()))
			.filter(this.searchablePropertyFilter)
			.map(d -> toFilterProperty(beanType, d))
			.collect(toList());

		final List<FilterProperty<?>> filterableProperties = grid.getColumns().stream()
			.map(c -> propertySet.getProperty(c.getKey()))
			.filter(Optional::isPresent).map(Optional::get)
			.filter(p -> isFilterable(p.getType()))
			.filter(this.filterablePropertyFilter)
			.map(d -> toFilterProperty(beanType, d))
			.collect(toList());

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
}

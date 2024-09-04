/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.filter;

import java.util.Collection;

import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.data.binder.BeanPropertySet;
import com.vaadin.flow.data.binder.PropertySet;


/**
 * @author XDEV Software
 *
 */
public class GridFilterSubjectFactory extends FilterSubjectFactory.Abstract<Grid<?>>
{
	public static GridFilterSubjectFactory New()
	{
		return new GridFilterSubjectFactory(null, null);
	}
	
	public static GridFilterSubjectFactory New(
		final Collection<String> searchableProperties,
		final Collection<String> filterableProperties)
	{
		return new GridFilterSubjectFactory(
			searchableProperties,
			filterableProperties);
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
	
	protected GridFilterSubjectFactory(
		final Collection<String> searchableProperties,
		final Collection<String> filterableProperties)
	{
		super(searchableProperties, filterableProperties);
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
		return createFilterSubjectForType(beanType);
	}
}

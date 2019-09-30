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

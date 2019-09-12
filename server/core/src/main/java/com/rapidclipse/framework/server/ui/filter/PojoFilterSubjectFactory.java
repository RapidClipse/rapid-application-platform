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

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;

import com.rapidclipse.framework.server.resources.CaptionUtils;


/**
 * @author XDEV Software
 *
 */
public class PojoFilterSubjectFactory extends FilterSubjectFactory.Abstract<Object>
{
	public static PojoFilterSubjectFactory New()
	{
		return new PojoFilterSubjectFactory();
	}

	public static PojoFilterSubjectFactory New(
		final Predicate<PropertyDescriptor> searchablePropertyFilter,
		final Predicate<PropertyDescriptor> filterablePropertyFilter)
	{
		return new PojoFilterSubjectFactory(searchablePropertyFilter, filterablePropertyFilter);
	}

	public static PojoFilterSubjectFactory New(
		final Collection<String> searchableProperties,
		final Collection<String> filterableProperties)
	{
		return new PojoFilterSubjectFactory(
			d -> searchableProperties.contains(d.getName()),
			d -> filterableProperties.contains(d.getName()));
	}

	public static FilterSubject CreateFilterSubject(final Class<?> type)
	{
		return New().createFilterSubject(type);
	}

	public static FilterSubject CreateFilterSubject(
		final Class<?> type,
		final Collection<String> searchableProperties,
		final Collection<String> filterableProperties)
	{
		return New(searchableProperties, filterableProperties).createFilterSubject(type);
	}

	private final Predicate<PropertyDescriptor> searchablePropertyFilter;
	private final Predicate<PropertyDescriptor> filterablePropertyFilter;

	public PojoFilterSubjectFactory()
	{
		this(d -> true, d -> true);
	}

	public PojoFilterSubjectFactory(
		final Predicate<PropertyDescriptor> searchablePropertyFilter,
		final Predicate<PropertyDescriptor> filterablePropertyFilter)
	{
		super();
		
		this.searchablePropertyFilter = requireNonNull(searchablePropertyFilter);
		this.filterablePropertyFilter = requireNonNull(filterablePropertyFilter);
	}

	@Override
	public FilterSubject createFilterSubject(final Object source)
	{
		try
		{
			final Class<?> clazz =
				source instanceof Class ? (Class<?>)source : source.getClass();

			final PropertyDescriptor[] propertyDescriptors = Introspector.getBeanInfo(clazz)
				.getPropertyDescriptors();

			final List<FilterProperty<?>> searchableProperties = Arrays.stream(propertyDescriptors)
				.filter(d -> isSearchable(d.getPropertyType()))
				.filter(this.searchablePropertyFilter)
				.map(d -> toFilterProperty(clazz, d))
				.collect(toList());

			final List<FilterProperty<?>> filterableProperties = Arrays.stream(propertyDescriptors)
				.filter(d -> isFilterable(d.getPropertyType()))
				.filter(this.filterablePropertyFilter)
				.map(d -> toFilterProperty(clazz, d))
				.collect(toList());

			return FilterSubject.New(searchableProperties, filterableProperties);
		}
		catch(final IntrospectionException e)
		{
			throw new RuntimeException(e);
		}
	}

	protected FilterProperty<?> toFilterProperty(
		final Class<?> clazz,
		final PropertyDescriptor beanProperty)
	{
		final String name = beanProperty.getName();
		return FilterProperty.New(name, beanProperty.getPropertyType(),
			CaptionUtils.resolveCaption(clazz, name));
	}
}

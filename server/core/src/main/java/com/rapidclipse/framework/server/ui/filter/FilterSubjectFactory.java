/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.filter;

import static java.util.stream.Collectors.toList;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.rapidclipse.framework.server.util.BeanInfoUtils;


/**
 * @author XDEV Software
 *
 */
public interface FilterSubjectFactory<S> extends Serializable
{
	public FilterSubject createFilterSubject(S source);

	public static abstract class Abstract<S> implements FilterSubjectFactory<S>
	{
		private final Collection<String> searchablePropertyNames;
		private final Collection<String> filterablePropertyNames;

		protected Abstract(
			final Collection<String> searchablePropertyNames,
			final Collection<String> filterablePropertyNames)
		{
			super();

			this.searchablePropertyNames = searchablePropertyNames;
			this.filterablePropertyNames = filterablePropertyNames;
		}

		protected FilterSubject createFilterSubjectForType(final Class<?> type)
		{
			final List<FilterProperty<?>> searchableProperties = propertyStream(type, this.searchablePropertyNames)
				.filter(pair -> isSearchable(pair.getRight().getPropertyType()))
				.map(pair -> toFilterProperty(type, pair.getRight(), pair.getLeft()))
				.collect(toList());

			final List<FilterProperty<?>> filterableProperties = propertyStream(type, this.filterablePropertyNames)
				.filter(pair -> isFilterable(pair.getRight().getPropertyType()))
				.map(pair -> toFilterProperty(type, pair.getRight(), pair.getLeft()))
				.collect(toList());

			return FilterSubject.New(searchableProperties, filterableProperties);
		}

		protected Stream<Pair<String, PropertyDescriptor>>
			propertyStream(final Class<?> type, final Collection<String> propertyNames)
		{
			if(propertyNames == null)
			{
				try
				{
					final BeanInfo beanInfo = Introspector.getBeanInfo(type);
					return Arrays.stream(beanInfo.getPropertyDescriptors())
						.map(d -> Pair.of(d.getName(), d));
				}
				catch(final IntrospectionException e)
				{
					throw new RuntimeException(e);
				}
			}

			return propertyNames.stream()
				.map(property -> Pair.of(property, BeanInfoUtils.getPropertyDescriptor(type, property)));
		}

		protected FilterProperty<?> toFilterProperty(
			final Class<?> clazz,
			final PropertyDescriptor beanProperty,
			final String name)
		{
			return FilterProperty.New(name, beanProperty.getPropertyType(),
				CaptionUtils.resolveCaption(clazz, name));
		}

		protected boolean isSearchable(final Class<?> type)
		{
			return String.class.equals(type);
		}

		protected boolean isFilterable(final Class<?> type)
		{
			return !type.isArray();
		}
	}
}

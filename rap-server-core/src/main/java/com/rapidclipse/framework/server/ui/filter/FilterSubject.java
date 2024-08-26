/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.filter;

import static java.util.Collections.unmodifiableCollection;
import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.Collection;


/**
 * @author XDEV Software
 *
 */
public interface FilterSubject extends Serializable
{
	public default FilterProperty<?> searchableProperty(final Object identifier)
	{
		return searchableProperties().stream()
			.filter(property -> property.identifier().equals(identifier)).findFirst()
			.orElse(null);
	}

	public Collection<FilterProperty<?>> searchableProperties();

	public default FilterProperty<?> filterableProperty(final Object identifier)
	{
		return filterableProperties().stream()
			.filter(property -> property.identifier().equals(identifier)).findFirst()
			.orElse(null);
	}

	public Collection<FilterProperty<?>> filterableProperties();

	public static FilterSubject New(
		final Collection<FilterProperty<?>> searchableProperties,
		final Collection<FilterProperty<?>> filterableProperties)
	{
		return new Default(searchableProperties, filterableProperties);
	}

	public static class Default implements FilterSubject
	{
		private final Collection<FilterProperty<?>> searchableProperties;
		private final Collection<FilterProperty<?>> filterableProperties;

		protected Default(
			final Collection<FilterProperty<?>> searchableProperties,
			final Collection<FilterProperty<?>> filterableProperties)
		{
			super();

			this.searchableProperties = unmodifiableCollection(
				requireNonNull(searchableProperties));
			this.filterableProperties = unmodifiableCollection(
				requireNonNull(filterableProperties));
		}

		@Override
		public Collection<FilterProperty<?>> searchableProperties()
		{
			return this.searchableProperties;
		}

		@Override
		public Collection<FilterProperty<?>> filterableProperties()
		{
			return this.filterableProperties;
		}
	}
}

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

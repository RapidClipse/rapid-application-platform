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


import static java.util.Collections.unmodifiableCollection;
import static java.util.Objects.requireNonNull;

import java.util.Collection;


/**
 * @author XDEV Software
 *
 */
public interface FilterSubject
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


	public static FilterSubject New(final Collection<FilterProperty<?>> searchableProperties,
			final Collection<FilterProperty<?>> filterableProperties)
	{
		return new Implementation(searchableProperties,filterableProperties);
	}



	public static class Implementation implements FilterSubject
	{
		private final Collection<FilterProperty<?>>	searchableProperties;
		private final Collection<FilterProperty<?>>	filterableProperties;
		
		
		public Implementation(final Collection<FilterProperty<?>> searchableProperties,
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

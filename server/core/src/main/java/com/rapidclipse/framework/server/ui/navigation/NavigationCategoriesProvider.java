/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.navigation;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.navigation.NavigationCategory;
import com.rapidclipse.framework.server.navigation.NavigationItem;


/**
 * @author XDEV Software
 *
 */
public interface NavigationCategoriesProvider extends Serializable
{
	public Collection<NavigationCategory> getRootCategories();
	
	public static NavigationCategoriesProvider New(final NavigationCategory... categories)
	{
		return () -> Arrays.asList(categories);
	}
	
	public static NavigationCategoriesProvider ForItems(final List<NavigationItem> items)
	{
		return new ItemBound(items);
	}
	
	public static class ItemBound implements NavigationCategoriesProvider
	{
		private final List<NavigationItem> items;
		
		protected ItemBound(final List<NavigationItem> items)
		{
			super();
			
			this.items = items;
		}
		
		@Override
		public Collection<NavigationCategory> getRootCategories()
		{
			return this.items.stream()
				.map(NavigationItem::category)
				.filter(c -> !StringUtils.isBlank(c))
				.distinct()
				.map(c -> NavigationCategory.New(null, c))
				.collect(Collectors.toList());
		}
	}
}

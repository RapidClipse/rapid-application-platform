/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

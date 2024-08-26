/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.navigation;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import com.rapidclipse.framework.server.navigation.NavigationCategory;
import com.rapidclipse.framework.server.navigation.NavigationItem;


/**
 * @author XDEV Software
 *
 */
public interface NavigationHierarchyProvider extends NavigationCategoriesProvider
{
	public Collection<NavigationCategory> getChildCategories(NavigationCategory category);
	
	public static NavigationHierarchyProvider ForItems(final List<NavigationItem> items)
	{
		return new Plain(items);
	}
	
	public static class Plain extends NavigationCategoriesProvider.ItemBound implements NavigationHierarchyProvider
	{
		protected Plain(final List<NavigationItem> items)
		{
			super(items);
		}
		
		@Override
		public Collection<NavigationCategory> getChildCategories(final NavigationCategory category)
		{
			return null;
		}
	}
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public default Builder addRoots(final NavigationCategory... roots)
		{
			return addRoots(Arrays.asList(roots));
		}
		
		public default Builder addRoots(final Collection<NavigationCategory> roots)
		{
			return addChildren(null, roots);
		}
		
		public default Builder addChildren(final NavigationCategory parent, final NavigationCategory... children)
		{
			return addChildren(parent, Arrays.asList(children));
		}
		
		public Builder addChildren(NavigationCategory parent, Collection<NavigationCategory> children);
		
		public NavigationHierarchyProvider build();
		
		public static class Default implements Builder
		{
			private final Map<NavigationCategory, Collection<NavigationCategory>> map = new LinkedHashMap<>();
			
			protected Default()
			{
				super();
			}
			
			@Override
			public Builder addChildren(final NavigationCategory parent, final Collection<NavigationCategory> children)
			{
				this.map.computeIfAbsent(parent, key -> new LinkedHashSet<>()).addAll(children);
				return this;
			}
			
			@Override
			public NavigationHierarchyProvider build()
			{
				final Map<NavigationCategory, Collection<NavigationCategory>> map = this.map;
				
				return new NavigationHierarchyProvider()
				{
					@Override
					public Collection<NavigationCategory> getRootCategories()
					{
						return map.get(null);
					}
					
					@Override
					public Collection<NavigationCategory> getChildCategories(final NavigationCategory category)
					{
						return map.get(category);
					}
				};
			}
		}
	}
}

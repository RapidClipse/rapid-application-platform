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

package com.rapidclipse.framework.server.ui.navigation;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.navigation.NavigationItem;


/**
 * @author XDEV Software
 *
 */
public interface NavigationItemHierarchyProvider extends Serializable
{
	public Iterable<String> getRootCategories();
	
	public Iterable<String> getChildCategories(String category);
	
	public static NavigationItemHierarchyProvider Plain(final List<NavigationItem> items)
	{
		return new Plain(items);
	}
	
	public static class Plain implements NavigationItemHierarchyProvider
	{
		private final List<NavigationItem> items;
		
		protected Plain(final List<NavigationItem> items)
		{
			super();
			
			this.items = items;
		}
		
		@Override
		public Iterable<String> getRootCategories()
		{
			return this.items.stream()
				.map(NavigationItem::category)
				.filter(c -> !StringUtils.isBlank(c))
				.distinct()
				.collect(Collectors.toList());
		}
		
		@Override
		public Iterable<String> getChildCategories(final String category)
		{
			return null;
		}
	}
	
	public static Builder Builder()
	{
		return new Builder.Implementation();
	}
	
	public static interface Builder
	{
		public default Builder addRoots(final String... roots)
		{
			return addRoots(Arrays.asList(roots));
		}
		
		public default Builder addRoots(final Collection<String> roots)
		{
			return addChildren(null, roots);
		}
		
		public default Builder addChildren(final String parent, final String... children)
		{
			return addChildren(parent, Arrays.asList(children));
		}
		
		public Builder addChildren(String parent, Collection<String> children);
		
		public NavigationItemHierarchyProvider build();
		
		public static class Implementation implements Builder
		{
			private final Map<String, Collection<String>> map = new LinkedHashMap<>();
			
			protected Implementation()
			{
				super();
			}
			
			@Override
			public Builder addChildren(final String parent, final Collection<String> children)
			{
				this.map.computeIfAbsent(parent, key -> new LinkedHashSet<>()).addAll(children);
				return this;
			}
			
			@Override
			public NavigationItemHierarchyProvider build()
			{
				final Map<String, Collection<String>> map = this.map;
				
				return new NavigationItemHierarchyProvider()
				{
					@Override
					public Iterable<String> getRootCategories()
					{
						return map.get(null);
					}
					
					@Override
					public Iterable<String> getChildCategories(final String category)
					{
						return map.get(category);
					}
				};
			}
		}
	}
}

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
	public Iterable<NavigationCategory> getChildCategories(NavigationCategory category);

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
		public Iterable<NavigationCategory> getChildCategories(final NavigationCategory category)
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

		public static class Implementation implements Builder
		{
			private final Map<NavigationCategory, Collection<NavigationCategory>> map = new LinkedHashMap<>();

			protected Implementation()
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
					public Iterable<NavigationCategory> getRootCategories()
					{
						return map.get(null);
					}

					@Override
					public Iterable<NavigationCategory> getChildCategories(final NavigationCategory category)
					{
						return map.get(category);
					}
				};
			}
		}
	}
}

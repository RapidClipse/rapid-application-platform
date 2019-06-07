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
import java.util.List;
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
}

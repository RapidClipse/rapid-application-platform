/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.navigation;

import com.rapidclipse.framework.server.navigation.NavigationCategory;
import com.rapidclipse.framework.server.navigation.NavigationElement;
import com.rapidclipse.framework.server.navigation.NavigationItem;
import com.vaadin.flow.function.SerializableComparator;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface NavigationHierarchyLevelSorter extends SerializableComparator<NavigationElement>
{
	public static NavigationHierarchyLevelSorter Default()
	{
		return CategoriesFirst();
	}

	public static NavigationHierarchyLevelSorter CategoriesFirst()
	{
		return (e1, e2) -> {
			
			if(e1 instanceof NavigationCategory)
			{
				return e2 instanceof NavigationCategory ? 0 : -1;
			}
			if(e2 instanceof NavigationCategory)
			{
				return e1 instanceof NavigationCategory ? 0 : 1;
			}
			return 0;
		};
	}

	public static NavigationHierarchyLevelSorter ItemsFirst()
	{
		return (e1, e2) -> {
			
			if(e1 instanceof NavigationItem)
			{
				return e2 instanceof NavigationItem ? 0 : -1;
			}
			if(e2 instanceof NavigationCategory)
			{
				return e1 instanceof NavigationItem ? 0 : 1;
			}
			return 0;
		};
	}
}

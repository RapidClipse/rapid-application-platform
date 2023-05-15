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

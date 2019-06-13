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

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.navigation;

import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.function.SerializablePredicate;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface NavigationItemFilter extends SerializablePredicate<NavigationItem>
{
	public final static NavigationItemFilter ALL     = item -> true;
	public final static NavigationItemFilter DEFAULT = item -> !item.isHidden();

	public static SerializablePredicate<NavigationItem> RegisteredFilters()
	{
		SerializablePredicate<NavigationItem> predicate = DEFAULT;
		for(final NavigationItemFilter filter : ServiceLoader.forType(NavigationItemFilter.class).services())
		{
			predicate = predicate.and(filter);
		}
		return predicate;
	}
}

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

package com.rapidclipse.framework.server.navigation;

import java.util.function.Predicate;

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface NavigationItemFilter extends Predicate<NavigationItem>
{
	public final static NavigationItemFilter ALL = item -> true;
	
	public static Predicate<NavigationItem> RegisteredFilters()
	{
		Predicate<NavigationItem> predicate = ALL;
		for(final NavigationItemFilter filter : ServiceLoader.forType(NavigationItemFilter.class).services())
		{
			predicate = predicate.and(filter);
		}
		return predicate;
	}
}

/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.navigation;

import static java.util.Objects.requireNonNull;

import com.vaadin.flow.component.Component;


/**
 * @author XDEV Software
 *
 */
public class AuthNavigationItemFilter implements NavigationItemFilter
{
	private AuthNavigationController controller          = AuthNavigationController.Default();
	private boolean                  hideUnauthenticated = true;
	private boolean                  hideUnauthorized    = true;

	public AuthNavigationItemFilter()
	{
		super();
	}

	public AuthNavigationController getController()
	{
		return this.controller;
	}

	public AuthNavigationItemFilter setController(final AuthNavigationController controller)
	{
		this.controller = requireNonNull(controller);
		return this;
	}

	public boolean isHideUnauthenticated()
	{
		return this.hideUnauthenticated;
	}

	public AuthNavigationItemFilter setHideUnauthenticated(final boolean hideUnauthenticated)
	{
		this.hideUnauthenticated = hideUnauthenticated;
		return this;
	}

	public boolean isHideUnauthorized()
	{
		return this.hideUnauthorized;
	}

	public AuthNavigationItemFilter setHideUnauthorized(final boolean hideUnauthorized)
	{
		this.hideUnauthorized = hideUnauthorized;
		return this;
	}

	@Override
	public boolean test(final NavigationItem item)
	{
		final Class<? extends Component> target = item.routeData().getNavigationTarget();
		
		if(this.hideUnauthenticated && !this.controller.isAuthenticated(target))
		{
			return false;
		}
		
		if(this.hideUnauthorized && !this.controller.isAuthorized(target))
		{
			return false;
		}

		return true;
	}
}

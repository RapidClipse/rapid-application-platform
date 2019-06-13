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
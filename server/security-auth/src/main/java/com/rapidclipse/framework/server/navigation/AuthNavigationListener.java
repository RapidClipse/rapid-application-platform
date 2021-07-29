/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.navigation;

import com.rapidclipse.framework.server.security.authentication.Authentication;
import com.rapidclipse.framework.server.security.authentication.UnauthenticatedNavigationRequestHandler;
import com.rapidclipse.framework.server.security.authorization.Authorization;
import com.rapidclipse.framework.server.security.authorization.UnauthorizedNavigationRequestHandler;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.BeforeEnterListener;
import com.vaadin.flow.router.ListenerPriority;


/**
 * @author XDEV Software
 *
 */
@ListenerPriority(Integer.MAX_VALUE)
public class AuthNavigationListener implements BeforeEnterListener
{
	private final AuthNavigationController controller = AuthNavigationController.Default();
	
	public AuthNavigationListener()
	{
		super();
	}
	
	@Override
	public void beforeEnter(final BeforeEnterEvent event)
	{
		if(!this.controller.isAuthenticated(event.getNavigationTarget()))
		{
			final UnauthenticatedNavigationRequestHandler handler = Authentication
				.getUnauthenticatedNavigationRequestHandler();
			if(handler != null)
			{
				handler.handle(event);
			}
		}
		else if(!this.controller.isAuthorized(event.getNavigationTarget()))
		{
			final UnauthorizedNavigationRequestHandler handler = Authorization
				.getUnauthorizedNavigationRequestHandler();
			if(handler != null)
			{
				handler.handle(event);
			}
		}
	}
}

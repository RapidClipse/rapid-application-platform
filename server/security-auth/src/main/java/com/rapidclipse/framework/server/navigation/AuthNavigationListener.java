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

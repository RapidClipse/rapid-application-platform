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

package com.rapidclipse.framework.server.security;

import java.util.Collection;

import com.rapidclipse.framework.security.authorization.Resource;
import com.rapidclipse.framework.security.authorization.Subject;
import com.rapidclipse.framework.server.security.authentication.Authentication;
import com.rapidclipse.framework.server.security.authentication.UnauthenticatedNavigationRequestHandler;
import com.rapidclipse.framework.server.security.authentication.annotations.AccessibleView;
import com.rapidclipse.framework.server.security.authorization.Authorization;
import com.rapidclipse.framework.server.security.authorization.UnauthorizedNavigationRequestHandler;
import com.rapidclipse.framework.server.util.ReflectionUtils;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.BeforeEnterListener;
import com.vaadin.flow.router.ListenerPriority;


/**
 * @author XDEV Software
 *
 */
@ListenerPriority(Integer.MAX_VALUE)
public class AuthNavigationController implements BeforeEnterListener
{
	@Override
	public void beforeEnter(final BeforeEnterEvent event)
	{
		if(!isAuthenticated(event))
		{
			final UnauthenticatedNavigationRequestHandler handler = Authentication
				.getUnauthenticatedNavigationRequestHandler();
			if(handler != null)
			{
				handler.handle(event);
			}
		}
		else if(!isAuthorized(event))
		{
			final UnauthorizedNavigationRequestHandler handler = Authorization
				.getUnauthorizedNavigationRequestHandler();
			if(handler != null)
			{
				handler.handle(event);
			}
		}
	}
	
	protected boolean isAuthenticated(final BeforeEnterEvent event)
	{
		return ReflectionUtils.isAnnotationPresent(event.getNavigationTarget(), AccessibleView.class)
			|| Authentication.getUser() != null;
	}
	
	protected boolean isAuthorized(final BeforeEnterEvent event)
	{
		final Collection<Resource> resources = getRequiredResources(event);
		if(resources == null || resources.isEmpty())
		{
			return true;
		}
		
		final Subject user = Authentication.getUser();
		if(user == null)
		{
			return false;
		}
		
		for(final Resource resource : resources)
		{
			if(!user.hasPermission(resource))
			{
				return false;
			}
		}
		
		return true;
	}
	
	protected Collection<Resource> getRequiredResources(final BeforeEnterEvent event)
	{
		return Authorization.getRouteResourcesProvider()
			.getResourcesFor(event.getLocation(), event.getNavigationTarget());
	}
}

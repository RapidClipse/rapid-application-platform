/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.security;


import java.util.Collection;

import com.rapidclipse.framework.security.authorization.Resource;
import com.rapidclipse.framework.security.authorization.Subject;
import com.rapidclipse.framework.server.security.authentication.AccessibleView;
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
		return AccessibleView.class.isAssignableFrom(event.getNavigationTarget())
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
		// TODO view to resource binding
		throw new Error("Not implemented yet");
	}
}

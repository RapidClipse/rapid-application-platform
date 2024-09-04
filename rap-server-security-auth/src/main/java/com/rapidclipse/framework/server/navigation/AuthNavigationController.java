/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.navigation;

import java.io.Serializable;
import java.util.Collection;

import com.rapidclipse.framework.security.authorization.Resource;
import com.rapidclipse.framework.security.authorization.Subject;
import com.rapidclipse.framework.server.security.authentication.AccessibleView;
import com.rapidclipse.framework.server.security.authentication.Authentication;
import com.rapidclipse.framework.server.security.authorization.Authorization;
import com.rapidclipse.framework.server.util.ReflectionUtils;
import com.vaadin.flow.router.HasErrorParameter;


/**
 * @author XDEV Software
 *
 */
public interface AuthNavigationController extends Serializable
{
	public boolean isAuthenticated(final Class<?> target);

	public boolean isAuthorized(final Class<?> target);
	
	public static AuthNavigationController Default()
	{
		return new Default();
	}

	public static class Default implements AuthNavigationController
	{
		protected Default()
		{
			super();
		}

		@Override
		public boolean isAuthenticated(final Class<?> target)
		{
			return ReflectionUtils.isAnnotationPresent(target, AccessibleView.class)
				|| HasErrorParameter.class.isAssignableFrom(target)
				|| Authentication.getUser() != null;
		}

		@Override
		public boolean isAuthorized(final Class<?> target)
		{
			final Collection<Resource> resources = Authorization.getRouteResourcesProvider().getResourcesFor(target);
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
	}
}

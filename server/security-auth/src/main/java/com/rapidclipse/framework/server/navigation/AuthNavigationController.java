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

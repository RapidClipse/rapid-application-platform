/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
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

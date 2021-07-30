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
package com.rapidclipse.framework.server.security.authorization;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Collectors;

import com.rapidclipse.framework.security.authorization.Resource;


/**
 * Lookup for resources within routes. Default implementation uses the {@link Resources} annotation.
 *
 * @see Resource
 * @see Resources
 * @see Authorization#setRouteResourcesProvider(RouteResourcesProvider)
 *
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface RouteResourcesProvider
{
	public Collection<Resource> getResourcesFor(Class<?> target);

	public static RouteResourcesProvider Default()
	{
		return new AnnotationBased();
	}

	public static class AnnotationBased implements RouteResourcesProvider
	{
		@Override
		public Collection<Resource> getResourcesFor(final Class<?> target)
		{
			final Resources resources = target.getAnnotation(Resources.class);
			if(resources != null)
			{
				final String[] names = resources.value();
				if(names != null && names.length > 0)
				{
					return Arrays.stream(names).map(Authorization::resource).collect(Collectors.toList());
				}
			}

			return null;
		}
	}
}

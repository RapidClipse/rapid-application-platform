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

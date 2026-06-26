/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.security.authorization;

/**
 * Function type that provides {@link Permission} instances.
 * For details, see {@link #providePermission(Resource, Integer)}.
 *
 * @author XDEV Software (TM)
 */
@FunctionalInterface
public interface PermissionProvider
{
	/**
	 * Provides a suitable {@link Permission} instance for the passed {@link Resource} instance and
	 * factor value. Providing means to either return a fitting instance or create a new one.
	 *
	 * @param resource
	 *            the {@link Resource} instance to be associated.
	 * @param factor
	 *            the factor of the access to the passed {@link Resource} instance.
	 * @return a {@link Permission} instance satisfiying the specified values.
	 */
	public Permission providePermission(Resource resource, Integer factor);
	
	/**
	 * Provides a permission for the passed {@link Resource} instance and a factor of 0.
	 *
	 * @param resource
	 *            the {@link Resource} instance to be associated.
	 * @return a {@link Permission} instance associated with the passed {@link Resource} instance.
	 * @see #providePermission(Resource, Integer)
	 */
	public default Permission providePermission(final Resource resource)
	{
		return this.providePermission(resource, 0);
	}
	
}

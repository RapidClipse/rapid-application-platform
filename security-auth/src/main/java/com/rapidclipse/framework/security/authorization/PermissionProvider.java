/*-
 * ---
 * Rapid Application Platform / Security / Authentication and Authorization
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
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

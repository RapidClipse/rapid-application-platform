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

import java.util.Set;


/**
 * Function type that provides {@link Role} instances.
 * For details, see {@link #provideRole(Role, String, Set, Set)}.
 *
 * @see RoleUpdater
 * @author XDEV Software (TM)
 */
@FunctionalInterface
public interface RoleProvider
{
	/**
	 * Provides a suitable {@link Role} instance for the passed role name, parent role and permission names.
	 * Providing means to either validate and return a fitting instance or create a new one.
	 * See {@link RoleUpdater} for setting the actual role and permission values.
	 *
	 * @param role
	 *            a potentially already existing {@link Role} instance for the passed name or <tt>null</tt>.
	 * @param name
	 *            the name identifiying the role.
	 * @param parentRoles
	 *            the names of the role's parent roles for validation purposes.
	 * @param permissions
	 *            the names of the role's explicit permissions for validation purposes.
	 * @return a {@link Role} instance suitable for the specified values, either already existing or newly created.
	 */
	public Role provideRole(Role existingRole, String name, Set<String> parentRoles, Set<String> permissions);
}

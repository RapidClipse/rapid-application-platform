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

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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.security.authorization;

import static java.util.Objects.requireNonNull;

import java.util.Map;
import java.util.Set;


/**
 * Intermediate data structure that represents authorization definition
 * information in a general purpose, yet efficiently processable format.
 *
 * @author XDEV Software (TM)
 */
public interface AuthorizationConfiguration
{
	/**
	 * The child resource names (value) for every valid resource (keys). The key
	 * collection of the returned map also represents the complete set of valid
	 * resources.
	 *
	 * @return a table representing all valid resources and their defined child
	 *         resources.
	 */
	public Map<String, ? extends Set<String>> resourceResources();

	/**
	 * The parent role names (value) for every valid role (keys). The key
	 * collection of the returned map also represents the complete set of valid
	 * roles.
	 *
	 * @return a table representing all valid roles and their parent roles.
	 * @see #roleRoles()
	 */
	public Map<String, ? extends Set<String>> roleRoles();

	/**
	 * The explicitely defined permissions (value) for every valid role (keys).
	 * Every permission is represented by its name and its associated factor.
	 * The key collection of the returned map also represents the complete set
	 * of valid roles.
	 *
	 * @return a table representing all valid roles and their defined
	 *         permissions.
	 * @see #rolePermissions()
	 */
	public Map<String, ? extends Map<String, Integer>> rolePermissions();

	/**
	 * The role names (value) for every valid subject (keys). The key collection
	 * of the returned map also represents the complete set of valid subjects.
	 *
	 * @return a table representing all valid subjects and their defined roles.
	 */
	public Map<String, ? extends Set<String>> subjectRoles();

	/**
	 * Creates a new {@link AuthorizationConfiguration} instance from the passed
	 * parts.
	 *
	 * @param resourceResources
	 *            see {@link #resourceResources()}
	 * @param roleRoles
	 *            see {@link #roleRoles()}
	 * @param rolePermissions
	 *            see {@link #rolePermissions()}
	 * @param subjectRoles
	 *            see {@link #subjectRoles()}
	 * @return a new {@link AuthorizationConfiguration} instance
	 */
	public static AuthorizationConfiguration New(
		final Map<String, ? extends Set<String>> resourceResources,
		final Map<String, ? extends Set<String>> roleRoles,
		final Map<String, ? extends Map<String, Integer>> rolePermissions,
		final Map<String, ? extends Set<String>> subjectRoles)
	{
		return new Default(requireNonNull(resourceResources), requireNonNull(roleRoles),
			requireNonNull(rolePermissions), requireNonNull(subjectRoles));
	}

	/**
	 * A simple immutable {@link AuthorizationConfiguration} default
	 * implementation.
	 *
	 * @author XDEV Software (TM)
	 */
	public final class Default implements AuthorizationConfiguration
	{
		////////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////

		private final Map<String, ? extends Set<String>>          resourceResources;
		private final Map<String, ? extends Set<String>>          roleRoles;
		private final Map<String, ? extends Map<String, Integer>> rolePermissions;
		private final Map<String, ? extends Set<String>>          subjectRoles;

		////////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////

		/**
		 * Implementation detail constructor that might change in the future.
		 */
		protected Default(
			final Map<String, ? extends Set<String>> resourceResources,
			final Map<String, ? extends Set<String>> roleRoles,
			final Map<String, ? extends Map<String, Integer>> rolePermissions,
			final Map<String, ? extends Set<String>> subjectRoles)
		{
			super();
			this.resourceResources = resourceResources;
			this.roleRoles         = roleRoles;
			this.rolePermissions   = rolePermissions;
			this.subjectRoles      = subjectRoles;
		}

		////////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Map<String, ? extends Set<String>> resourceResources()
		{
			return this.resourceResources;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Map<String, ? extends Set<String>> roleRoles()
		{
			return this.roleRoles;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Map<String, ? extends Map<String, Integer>> rolePermissions()
		{
			return this.rolePermissions;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Map<String, ? extends Set<String>> subjectRoles()
		{
			return this.subjectRoles;
		}

	}

}

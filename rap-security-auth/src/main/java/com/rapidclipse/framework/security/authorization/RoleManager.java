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

import static java.util.Objects.requireNonNull;

import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.security.util.LockedMap;


/**
 * Manager type that extends {@link RoleRegistry} with functionality for mutating (managing) the registered entries.
 *
 * @author XDEV Software (TM)
 */
public interface RoleManager extends RoleRegistry
{
	/**
	 * Provides access to the internal registry to be used in a safe way according to the internal
	 * logic. Note that this method does not necessarily have to return the actual map instance but
	 * potentially only a relay e.g. a {@link LockedMap}.
	 * <p>
	 * Rationale for this approach:
	 * The obvious (and naive) concept would be to implement a registerRole() method.
	 * But as a consequence, that would also require a removeRole() method, then a roleSize() and iterateRoles() method
	 * and then it becomes clear that managing roles (in an efficient, comfortable way) requires a complete collection
	 * logic. So instead of reimplementing a complete collection in every managing type, the managing type might as well
	 * provide access to its internal collection, however in a safe way (e.g. wrapped in mutex logic and/or via a relay
	 * instance with hooked-in logic).
	 *
	 * @return an accessing instance to the internal registry.
	 */
	@Override
	public Map<String, Role> roles();

	/**
	 * Creates a new {@link RoleManager} instance with no entries and an exclusive locking instance.
	 *
	 * @return a new {@link RoleManager} instance
	 */
	public static RoleManager New()
	{
		return New(new Object());
	}

	/**
	 * Creates a new {@link RoleManager} instance with no entries and the passed instance as a shared locking instance.
	 *
	 * @param registryLock
	 *            the shared locking instance to be used.
	 * @return a new {@link RoleManager} instance
	 */
	public static RoleManager New(final Object registryLock)
	{
		return new Default(
			requireNonNull(registryLock),
			new HashMap<>());
	}

	/**
	 * Creates a new {@link RoleManager} instance with the passed map used as its internal entries datastructure
	 * and an exclusive locking instance.
	 *
	 * @param map
	 *            the entries datastructure to be used internally.
	 * @return a new {@link RoleManager} instance.
	 */
	public static RoleManager New(final Map<String, Role> map)
	{
		return new Default(
			new Object(),
			requireNonNull(map));
	}

	/**
	 * Creates a new {@link RoleManager} instance with the passed map used as its internal entries datastructure
	 * and the passed instance as a shared locking instance.
	 *
	 * @param registryLock
	 *            the shared locking instance to be used.
	 * @param map
	 *            the entries datastructure to be used internally.
	 * @return a new {@link RoleManager} instance.
	 */
	public static RoleManager New(final Object registryLock, final Map<String, Role> map)
	{
		return new Default(
			requireNonNull(registryLock),
			requireNonNull(map));
	}

	/**
	 * A simple {@link RoleManager} default implementation that uses a shared synchronization lock and a
	 * {@link LockedMap} implementation to allow locking-supervised access to the registry entries.
	 *
	 * @author XDEV Software (TM)
	 */
	public final class Default implements RoleManager
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////

		private final Map<String, Role>       map;
		private final Object                  registryLock;
		private final LockedMap<String, Role> lockedMap;
		private final RoleRegistry            roleRegistry;

		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////

		/**
		 * Implementation detail constructor that might change in the future.
		 */
		protected Default(final Object registryLock, final Map<String, Role> map)
		{
			super();
			this.registryLock = registryLock;
			this.map          = map;
			this.lockedMap    = LockedMap.New(this.map, registryLock);
			this.roleRegistry = RoleRegistry.New(this.map, registryLock);
		}

		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Map<String, Role> roles()
		{
			return this.lockedMap;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Role role(final String roleName)
		{
			return this.roleRegistry.role(roleName);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Object lockRoleRegistry()
		{
			return this.registryLock;
		}

	}

}

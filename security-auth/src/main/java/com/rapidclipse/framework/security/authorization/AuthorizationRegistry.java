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
package com.rapidclipse.framework.security.authorization;

import java.util.Collection;
import java.util.Map;


/**
 * Composite type for centralized handling of {@link Permission}, {@link Role} and {@link Subject} instances.
 *
 * @author XDEV Software (TM)
 */
public interface AuthorizationRegistry extends PermissionRegistry, RoleRegistry, SubjectRegistry
{
	/**
	 * Creates a new {@link AuthorizationRegistry} instance from the passed sub-registries.
	 *
	 * @param permissionRegistry
	 *            the {@link PermissionRegistry} to be used.
	 * @param roleRegistry
	 *            the {@link RoleRegistry} to be used.
	 * @param subjectRegistry
	 *            the {@link SubjectRegistry} to be used.
	 * @return
	 */
	public static AuthorizationRegistry New(
		final PermissionRegistry permissionRegistry,
		final RoleRegistry roleRegistry,
		final SubjectRegistry subjectRegistry)
	{
		return new Implementation(permissionRegistry, roleRegistry, subjectRegistry);
	}
	
	/**
	 * Creates a new {@link AuthorizationRegistry} instance with the passed authorization entity instances as
	 * the entries for the according sub-registries and an internally created exclusive synchronization instance
	 * used for all sub-registries.
	 *
	 * @param permissions
	 *            the {@link Permission} instances to be registered internally.
	 * @param roles
	 *            the {@link Role} instances to be registered internally.
	 * @param subjects
	 *            the {@link Subject} instances to be registered internally.
	 * @return a new {@link AuthorizationRegistry} instance.
	 */
	public static AuthorizationRegistry New(
		final Collection<? extends Permission> permissions,
		final Collection<? extends Role> roles,
		final Collection<? extends Subject> subjects)
	{
		return New(permissions, roles, subjects, new Object());
	}
	
	/**
	 * Creates a new {@link AuthorizationRegistry} instance with the passed authorization entity instances as
	 * the entries for the according sub-registries and an internally created exclusive synchronization instance
	 * used for all sub-registries.
	 *
	 * @param permissions
	 *            the {@link Permission} instances to be registered internally.
	 * @param roles
	 *            the {@link Role} instances to be registered internally.
	 * @param subjects
	 *            the {@link Subject} instances to be registered internally.
	 * @param sharedLock
	 *            the instance used for locking by all sub-registries.
	 * @return a new {@link AuthorizationRegistry} instance.
	 */
	public static AuthorizationRegistry New(
		final Collection<? extends Permission> permissions,
		final Collection<? extends Role> roles,
		final Collection<? extends Subject> subjects,
		final Object sharedLock)
	{
		return new Implementation(
			PermissionRegistry.New(permissions, sharedLock),
			RoleRegistry.New(roles, sharedLock),
			SubjectRegistry.New(subjects, sharedLock));
	}
	
	/**
	 * A simple {@link AuthorizationRegistry} default implementation that is comprised of delegate
	 * {@link PermissionRegistry}, {@link RoleRegistry} and {@link SubjectRegistry} instances.
	 *
	 * @author XDEV Software (TM)
	 */
	public class Implementation implements AuthorizationRegistry
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////
		
		final PermissionRegistry permissionRegistry;
		final RoleRegistry       roleRegistry;
		final SubjectRegistry    subjectRegistry;
		
		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////
		
		/**
		 * Implementation detail constructor that might change in the future.
		 */
		Implementation(
			final PermissionRegistry permissionRegistry,
			final RoleRegistry roleRegistry,
			final SubjectRegistry subjectRegistry)
		{
			super();
			this.permissionRegistry = permissionRegistry;
			this.roleRegistry       = roleRegistry;
			this.subjectRegistry    = subjectRegistry;
		}
		
		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Permission permission(final Resource resource, final Integer factor)
		{
			return this.permissionRegistry.permission(resource, factor);
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
		public Subject subject(final String subjectName)
		{
			return this.subjectRegistry.subject(subjectName);
		}
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Map<String, Role> roles()
		{
			return this.roleRegistry.roles();
		}
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Object lockPermissionRegistry()
		{
			// will indirectly return the lock
			return this.permissionRegistry.lockPermissionRegistry();
		}
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Object lockRoleRegistry()
		{
			// will indirectly return the lock
			return this.roleRegistry.lockRoleRegistry();
		}
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Map<String, Subject> subjects()
		{
			return this.subjectRegistry.subjects();
		}
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Object lockSubjectRegistry()
		{
			// will indirectly return the lock
			return this.subjectRegistry.lockSubjectRegistry();
		}
		
	}
	
}

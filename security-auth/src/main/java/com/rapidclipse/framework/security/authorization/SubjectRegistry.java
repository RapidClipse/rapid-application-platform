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

import static java.util.Objects.requireNonNull;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.security.util.LockedMap;


/**
 * A registry type that provides a means for centralized lookup and iteration of known {@link Subject} instances.
 * Note that a registry conceptually only provides reading access, not modifying access to its contents. For
 * modifying operations, see {@link SubjectManager}.
 *
 * @author XDEV Software (TM)
 */
public interface SubjectRegistry
{
	/**
	 * Returns the {@link Subject} instance identified by the passed subject name.
	 *
	 * @param subjectName
	 *            the name identifying the desired {@link Subject} instance.
	 * @return the {@link Subject} instance identified by the passed name.
	 */
	public Subject subject(String subjectName);
	
	/**
	 * Returns a read-only map of all known {@link Subject} instances (values), identified by their names (keys).
	 *
	 * @return a read-only map containing all known subjects.
	 */
	public Map<String, Subject> subjects();
	
	/**
	 * Returns the lock instance that is internally used by this registry instance.
	 *
	 * @return the lock.
	 */
	public Object lockSubjectRegistry();
	
	/**
	 * Creates a new {@link SubjectRegistry} instance using the passed map instance as its internal datastructure
	 * and the passed registryLock instance as the synchronization lock for accessing the registry.
	 *
	 * @param registry
	 *            the map instance to be used as the internal datastructure.
	 * @param registryLock
	 *            the locking instance to be used to synchronize on for accessing the registry.
	 * @return a new {@link SubjectRegistry} instance using the passed instances.
	 */
	public static SubjectRegistry New(final Map<String, Subject> registry, final Object registryLock)
	{
		return new Implementation(
			requireNonNull(registry),
			requireNonNull(registryLock));
	}
	
	/**
	 * Creates a new {@link SubjectRegistry} instance using the passed map instance and a newly instantiated object
	 * to be used a synchronization lock.
	 *
	 * @param registry
	 *            the map instance to be used as the internal datastructure.
	 * @return a new {@link SubjectRegistry} instance using the passed instance.
	 */
	public static SubjectRegistry New(final Map<String, Subject> registry)
	{
		return New(registry, new Object());
	}
	
	/**
	 * Creates a new {@link SubjectRegistry} instance using the passed {@link Subject} instances to derive its
	 * internal datastructure from and the passed registryLock instance as the synchronization lock for
	 * accessing the registry.
	 *
	 * @param subjects
	 *            the {@link Subject} instances to derive the internal datastructure from.
	 * @param registryLock
	 *            the locking instance to be used to synchronize on for accessing the registry.
	 * @return a new {@link SubjectRegistry} instance using the passed instances.
	 */
	public static SubjectRegistry New(final Collection<? extends Subject> subjects, final Object registryLock)
	{
		return New(
			Implementation.buildRegistry(requireNonNull(subjects)),
			requireNonNull(registryLock));
	}
	
	/**
	 * Creates a new {@link SubjectRegistry} instance using the passed {@link Subject} instance to derive its
	 * internal datastructure from and a newly instantiated object to be used as the synchronization lock.
	 *
	 * @param subjects
	 *            the {@link Subject} instance to be used as the internal datastructure.
	 * @return a new {@link SubjectRegistry} instance using the passed instance.
	 */
	public static SubjectRegistry New(final Collection<? extends Subject> subjects)
	{
		return New(subjects, new Object());
	}
	
	/**
	 * A simple {@link SubjectRegistry} default implementation that synchronizes on a provided lock instance for
	 * accessing the internal registry in order to avoid concurrency issues while the internal datastructure is
	 * rebuilt.
	 *
	 * @author XDEV Software (TM)
	 */
	public final class Implementation implements SubjectRegistry
	{
		///////////////////////////////////////////////////////////////////////////
		// static methods //
		///////////////////
		
		static final Map<String, Subject> buildRegistry(final Collection<? extends Subject> subjects)
		{
			final HashMap<String, Subject> registry = new HashMap<>();
			
			subjects.forEach(subject -> registry.put(subject.name(), subject));
			
			return registry;
		}
		
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////
		
		/**
		 * The read-only subject-name-to-subject map used as an internal datastructure
		 */
		private final Map<String, Subject>       registry;
		
		/**
		 * The instance used to synchronize on. This may be any instance, even the map or registry instance itself.
		 */
		private final Object                     registryLock;
		
		/**
		 * A map wrapper implementation wrapping the actual registry map and using the registryLock instance to
		 * perform synchronization. Through this technique, the map can be accessed directly without losing the
		 * consistent concurrency protection achieve via the locking instance.
		 */
		private final LockedMap<String, Subject> lockedRegistry;
		
		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////
		
		/**
		 * Implementation-detail constructor that might change in the future.
		 *
		 * @param registry
		 *            the map instance to be used as the internal datastructure.
		 * @param registryLock
		 *            the locking instance to be used to synchronize on for accessing the registry.
		 */
		Implementation(final Map<String, Subject> registry, final Object registryLock)
		{
			super();
			this.registry       = registry;
			this.registryLock   = registryLock;
			this.lockedRegistry = LockedMap.New(this.registry, registryLock);
		}
		
		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Subject subject(final String subjectName)
		{
			// must lock registry instead of this as a supervising manager instance might modify registry
			synchronized(this.registryLock)
			{
				return this.registry.get(subjectName);
			}
		}
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Map<String, Subject> subjects()
		{
			return this.lockedRegistry;
		}
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Object lockSubjectRegistry()
		{
			return this.registryLock;
		}
		
	}
	
}
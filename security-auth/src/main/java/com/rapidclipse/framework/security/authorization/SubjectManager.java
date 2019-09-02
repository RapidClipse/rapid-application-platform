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

import static java.util.Objects.requireNonNull;

import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.security.util.LockedMap;


/**
 * Manager type that extends {@link SubjectRegistry} with functionality for mutating (managing) the registered entries.
 *
 * @author XDEV Software (TM)
 */
public interface SubjectManager extends SubjectRegistry
{
	/**
	 * Provides access to the registry entries to be used in a thread safe way according to the internal
	 * logic. Note that this method does not necessarily have to return the actual map instance but
	 * potentially only a relay e.g. a {@link LockedMap}.
	 * <p>
	 * Rationale for this approach:
	 * The obvious (and naive) concept would be to implement a registerSubject() method.
	 * But as a consequence, that would also require a removeSubject() method, then a subjectSize() and
	 * iterateSubjects()
	 * methods and then it becomes clear that managing subjects (in an efficient, comfortable way) requires a complete
	 * collection logic.
	 * So instead of reimplementing a complete collection in every managing type, the managing type might as well
	 * provide mutating access to its internal collection, however in a safe way (e.g. wrapped in locking logic
	 * and/or via a relay instance with hooked-in logic).
	 *
	 * @return an accessing instance to the internal registry.
	 */
	@Override
	public Map<String, Subject> subjects();
	
	/**
	 * Creates a new {@link SubjectManager} instance with no entries and an exclusive locking instance.
	 *
	 * @return a new {@link SubjectManager} instance
	 */
	public static SubjectManager New()
	{
		return New(new Object());
	}
	
	/**
	 * Creates a new {@link SubjectManager} instance with no entries and the passed instance as a shared locking
	 * instance.
	 *
	 * @param registryLock
	 *            the shared locking instance to be used.
	 * @return a new {@link SubjectManager} instance
	 */
	public static SubjectManager New(final Object registryLock)
	{
		return new Default(
			requireNonNull(registryLock),
			new HashMap<>());
	}
	
	/**
	 * Creates a new {@link SubjectManager} instance with the passed map used as its internal entries datastructure
	 * and an exclusive locking instance.
	 *
	 * @param map
	 *            the entries datastructure to be used internally.
	 * @return a new {@link SubjectManager} instance.
	 */
	public static SubjectManager New(final Map<String, Subject> map)
	{
		return new Default(
			new Object(),
			requireNonNull(map));
	}
	
	/**
	 * Creates a new {@link SubjectManager} instance with the passed map used as its internal entries datastructure
	 * and the passed instance as a shared locking instance.
	 *
	 * @param registryLock
	 *            the shared locking instance to be used.
	 * @param map
	 *            the entries datastructure to be used internally.
	 * @return a new {@link SubjectManager} instance.
	 */
	public static SubjectManager New(final Object registryLock, final Map<String, Subject> map)
	{
		return new Default(
			requireNonNull(registryLock),
			requireNonNull(map));
	}
	
	/**
	 * A simple {@link SubjectManager} default implementation that uses a shared synchronization lock and a
	 * {@link LockedMap} implementation to allow locking-supervised access to the registry entries.
	 *
	 * @author XDEV Software (TM)
	 */
	public final class Default implements SubjectManager
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////
		
		private final Object                     registryLock;
		private final LockedMap<String, Subject> lockedMap;
		private final SubjectRegistry            subjectRegistry;
		
		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////
		
		/**
		 * Implementation detail constructor that might change in the future.
		 */
		protected Default(final Object registryLock, final Map<String, Subject> map)
		{
			super();
			this.registryLock    = registryLock;
			this.lockedMap       = LockedMap.New(map, registryLock);
			this.subjectRegistry = SubjectRegistry.New(map, registryLock);
		}
		
		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////
		
		/**
		 * {@inheritDoc}
		 */
		@Override
		public Map<String, Subject> subjects()
		{
			return this.lockedMap;
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
		public Object lockSubjectRegistry()
		{
			return this.registryLock;
		}
		
	}
	
}

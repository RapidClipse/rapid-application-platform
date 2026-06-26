/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.security.authorization;

import java.util.Collection;
import java.util.Set;


/**
 * Function type for updating a passed {@link Subject} instance for a given subject name and collection of roles.
 * For more details, see {@link #updateSubject(Subject, String, Set)}.
 *
 * @author XDEV Software (TM)
 */
@FunctionalInterface
public interface SubjectUpdater
{
	/**
	 * Updates the passed {@link Subject} instance for the given subject name and collection of roles, where updating
	 * can mean anything from just validating to actually changing the passed subject's state with the passed content.
	 * 
	 * @param subject
	 *            the {@link Subject} instance to be updated.
	 * @param subjectName
	 *            the identifying name of the passed {@link Subject} instance.
	 * @param roles
	 *            the subject's defined roles to be used for the updating process.
	 *
	 * @return an updated {@link Subject} instance, potentially different from the passed instance.
	 */
	public Subject updateSubject(Subject subject, String subjectName, Set<Role> roles);
	
	/**
	 * Updating preparation callback hook that gets called once on the beginning of very updating process, before any
	 * authorization instances are updated.
	 * The default implementation of this method is empty.
	 * An example usage for this method is to create and store rollback information on the passed subjects.
	 *
	 * @param existingSubjects
	 *            the subjects already existing before the updating process.
	 */
	public default void prepareSubjectUpdate(final Collection<Subject> existingSubjects)
	{
		// no-op in default implementation
	}
	
	/**
	 * Update committing callback hook that gets called once at the end of very successful updating process, after all
	 * authorization instances are updated.
	 * The default implementation of this method is empty.
	 * An example usage for this method is to relay committing of changes to some other data structure or application
	 * part.
	 *
	 * @param updatedSubjects
	 *            the updated and potentially newly created subjects.
	 */
	public default void commitSubjectUpdate(final Collection<Subject> updatedSubjects)
	{
		// no-op in default implementation
	}
	
	/**
	 * Updating exception handling callback hook that gets called once if any {@link Exception} is encountered during
	 * the updating process.
	 * The default implementation of this method is empty.
	 * An example usage for this method is to rollback (revert) all mutated {@link Subject} instances that have
	 * already existed before the updating process.
	 *
	 * @param updatedSubjects
	 *            the updated and potentially newly created, empty or inconsistent subjects.
	 */
	public default void rollbackSubjectUpdate(final Collection<Subject> updatedSubjects, final Exception cause)
	{
		// no-op in default implementation
	}
	
	/**
	 * Updating preparation callback hook that gets called once at the end of the updating process in any case
	 * (both success and encountered exception).
	 * The default implementation of this method is empty.
	 * An example usage for this method is to clear any internally stored meta data (e.g. rollback information).
	 */
	public default void cleanupSubjectUpdate()
	{
		// no-op in default implementation
	}
	
}

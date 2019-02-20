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

import java.util.Set;


/**
 * Function type that provides {@link Resource} instances.
 * For details, see {@link #provideResource(Resource, String, Set)}.
 *
 * @author XDEV Software (TM)
 */
@FunctionalInterface
public interface ResourceProvider
{
	/**
	 * Provides a suitable {@link Resource} instance based on the passed resource name, a potentially already
	 * existing instance and the collection of names of children (possibly empty).
	 * Providing means either validating an already existing instance or creating a fitting new instance.
	 *
	 * @param existingInstance
	 *            the potentially already existing {@link Resource} instance for the passed name.
	 * @param factor
	 *            the factor of the access to the passed {@link Resource} instance.
	 *
	 * @return a new {@link Resource} instance that satisfies the specified values.
	 */
	public Resource provideResource(Resource existingInstance, String name, Set<String> children);
}

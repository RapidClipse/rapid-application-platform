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

/**
 * Contract for central resource collection enums.
 *
 * @author XDEV Software
 */
public interface ResourceEnum<E extends Enum<E> & ResourceEnum<E>>
{
	/**
	 * The resource's name
	 *
	 * @return the name of the resource
	 */
	public String resourceName();
	
	/**
	 * Gets the actual resource held by this enum entry.
	 *
	 * @return the actual resource
	 */
	public Resource resource();
}

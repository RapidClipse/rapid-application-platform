/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

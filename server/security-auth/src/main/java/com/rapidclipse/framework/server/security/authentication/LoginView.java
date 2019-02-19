/*-
 * ---
 * Rapid Application Platform / Server / Security / Authentication and Authorization
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

package com.rapidclipse.framework.server.security.authentication;

/**
 * A login view is used for authentication in an application.
 *
 * @author XDEV Software
 *
 */
public interface LoginView extends AccessibleView
{
	/**
	 * Returns the username of the login form.
	 *
	 * @return the username
	 */
	public String getUsername();
	
	/**
	 * Returns the password of the login form.
	 *
	 * @return the password
	 */
	public String getPassword();
}

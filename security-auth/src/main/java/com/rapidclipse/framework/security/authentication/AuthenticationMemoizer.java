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

package com.rapidclipse.framework.security.authentication;

/**
 * Credentials memoizer. Mostly used for the 'remember me' or 'stay logged in'
 * feature of an application.
 * <p>
 * For example if the login form's 'stay logged in' checkbox is selected and the
 * authentication was successful, call {@link #remember(Object)}.
 * <p>
 * Important: Don't forget to call {@link #forget(Object)} when the logout is
 * done.
 * <p>
 * Before showing the login form to the user {@link #lookup()} can be called to
 * retrieve the last used login credentials.
 *
 * @author XDEV Software
 */
public interface AuthenticationMemoizer<C>
{
	/**
	 * Remembers credentials. Best used after a successful authentication.
	 *
	 * @param credentials
	 *            the credentials to be remembered
	 */
	public void remember(C credentials);
	
	/**
	 * Forgets credentials. Best used after a successful logout.
	 *
	 * @param credentials
	 *            the credentials to forget
	 */
	public void forget(C credentials);
	
	/**
	 * Retrieves the last remembered credentials.
	 *
	 * @return the last remembered credentials or <code>null</code>
	 */
	public C lookup();
}

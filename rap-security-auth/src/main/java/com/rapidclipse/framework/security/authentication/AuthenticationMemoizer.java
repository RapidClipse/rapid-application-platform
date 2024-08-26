/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

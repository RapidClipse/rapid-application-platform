/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
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

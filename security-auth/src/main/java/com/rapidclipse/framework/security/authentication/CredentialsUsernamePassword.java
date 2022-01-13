/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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

import static java.util.Objects.requireNonNull;

import java.nio.charset.StandardCharsets;


/**
 * Trivial username / password string value tuple.
 *
 * @author XDEV Software (TM)
 */
public interface CredentialsUsernamePassword
{
	/**
	 * @return the username.
	 */
	public String username();

	/**
	 * @return the password.
	 */
	public byte[] password();

	/**
	 * Wraps the passed username and password strings as a new
	 * {@link CredentialsUsernamePassword} instance.
	 *
	 * @param username
	 *            the username to be wrapped.
	 * @param password
	 *            the password to be wrapped.
	 * @return a new {@link CredentialsUsernamePassword} instance containing the
	 *         passed credential values.
	 */
	public static CredentialsUsernamePassword New(
		final String username,
		final byte[] password)
	{
		return new Default(requireNonNull(username), requireNonNull(password));
	}

	/**
	 * Wraps the passed username and password strings as a new
	 * {@link CredentialsUsernamePassword} instance.
	 *
	 * @param username
	 *            the username to be wrapped.
	 * @param password
	 *            the password to be wrapped.
	 * @return a new {@link CredentialsUsernamePassword} instance containing the
	 *         passed credential values.
	 */
	public static CredentialsUsernamePassword New(
		final String username,
		final String password)
	{
		return new StringBased(requireNonNull(username),
			requireNonNull(password));
	}

	public final class StringBased implements CredentialsUsernamePassword
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////

		private final String username;
		private final String password;

		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////

		protected StringBased(final String username, final String password)
		{
			super();
			this.username = username;
			this.password = password;
		}

		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////

		@Override
		public String username()
		{
			return this.username;
		}

		@Override
		public byte[] password()
		{
			return this.password.getBytes(StandardCharsets.UTF_8);
		}

		@Override
		public String toString()
		{
			// intentionally don't give away the actual password.
			return this.username + " // (PWD length " + this.password.length() + ")";
		}
	}

	public final class Default implements CredentialsUsernamePassword
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////

		private final String username;
		private final byte[] password;

		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////

		protected Default(final String username, final byte[] password)
		{
			super();
			this.username = username;
			this.password = password;
		}

		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////

		@Override
		public String username()
		{
			return this.username;
		}

		@Override
		public byte[] password()
		{
			return this.password;
		}

		@Override
		public String toString()
		{
			// intentionally don't give away the actual password.
			return this.username + " // (PWD length " + this.password.length + ")";
		}
	}
}

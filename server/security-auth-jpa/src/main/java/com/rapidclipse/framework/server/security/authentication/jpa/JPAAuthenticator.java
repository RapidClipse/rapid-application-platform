/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.security.authentication.jpa;

import java.nio.charset.StandardCharsets;

import com.rapidclipse.framework.security.authentication.AuthenticationFailedException;
import com.rapidclipse.framework.security.authentication.Authenticator;
import com.rapidclipse.framework.security.authentication.CredentialsUsernamePassword;
import com.rapidclipse.framework.security.util.PasswordHasher;
import com.rapidclipse.framework.server.jpa.Jpa;


/**
 * @author XDEV Software
 */
public class JPAAuthenticator
	implements Authenticator<CredentialsUsernamePassword, CredentialsUsernamePassword>
{
	private final Class<? extends CredentialsUsernamePassword> authenticationEntityType;
	private PasswordHasher                                     passwordHasher = PasswordHasher
		.Sha2();

	/**
	 *
	 */
	public JPAAuthenticator(
		final Class<? extends CredentialsUsernamePassword> authenticationEntityType)
	{
		this.authenticationEntityType = authenticationEntityType;
	}

	public final CredentialsUsernamePassword authenticate(
		final String username,
		final String password)
		throws AuthenticationFailedException
	{
		return this.authenticate(CredentialsUsernamePassword.New(
			username,
			password.getBytes(StandardCharsets.UTF_8)));
	}

	@Override
	public CredentialsUsernamePassword authenticate(final CredentialsUsernamePassword credentials)
		throws AuthenticationFailedException
	{
		return checkCredentials(credentials);
	}

	protected CredentialsUsernamePassword checkCredentials(
		final CredentialsUsernamePassword credentials)
		throws AuthenticationFailedException
	{
		final CredentialsUsernamePassword found = Jpa
			.getDaoByEntityType(this.authenticationEntityType).findAll().stream()
			.filter(entity -> entity.username().equals(credentials.username())
				&& this.passwordHasher.validatePassword(credentials.password(), entity.password()))
			.findAny().orElse(null);
		if(found != null)
		{
			return found;
		}

		throw new AuthenticationFailedException();
	}

	public PasswordHasher getPasswordHasher()
	{
		return this.passwordHasher;
	}

	public void setPasswordHasher(final PasswordHasher passwordHasher)
	{
		this.passwordHasher = passwordHasher;
	}
}

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

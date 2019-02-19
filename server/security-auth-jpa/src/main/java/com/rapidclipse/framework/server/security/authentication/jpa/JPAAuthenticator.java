/*-
 * ---
 * Rapid Application Platform / Server / Security / Authentication and Authorization / JPA
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

package com.rapidclipse.framework.server.security.authentication.jpa;

import java.util.Arrays;

import com.rapidclipse.framework.security.authentication.AuthenticationFailedException;
import com.rapidclipse.framework.security.authentication.Authenticator;
import com.rapidclipse.framework.security.authentication.CredentialsUsernamePassword;
import com.rapidclipse.framework.security.util.PasswordHasher;
import com.rapidclipse.framework.server.persistence.jpa.Jpa;


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
		return this.authenticate(CredentialsUsernamePassword.New(username, password.getBytes()));
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
		final byte[]                      hashedPassword = this.passwordHasher.hashPassword(credentials.password());
		
		final CredentialsUsernamePassword found          = Jpa
			.getDaoByEntityType(this.authenticationEntityType).findAll().stream()
			.filter(entity -> entity.username().equals(credentials.username())
				&& Arrays.equals(hashedPassword, entity.password()))
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

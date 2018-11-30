/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.security.authentication.jpa;


import java.util.Arrays;

import software.xdev.rap.security.authentication.AuthenticationFailedException;
import software.xdev.rap.security.authentication.Authenticator;
import software.xdev.rap.security.authentication.CredentialsUsernamePassword;
import software.xdev.rap.security.util.PasswordHasher;
import software.xdev.rap.server.persistence.jpa.Jpa;


/**
 * @author XDEV Software
 */
public class JPAAuthenticator
		implements Authenticator<CredentialsUsernamePassword, CredentialsUsernamePassword>
{
	private final Class<? extends CredentialsUsernamePassword>	authenticationEntityType;
	private PasswordHasher										passwordHasher	= new PasswordHasher.SHA2();


	/**
	 *
	 */
	public JPAAuthenticator(
			final Class<? extends CredentialsUsernamePassword> authenticationEntityType)
	{
		this.authenticationEntityType = authenticationEntityType;
	}


	public final CredentialsUsernamePassword authenticate(final String username,
			final String password) throws AuthenticationFailedException
	{
		return this.authenticate(CredentialsUsernamePassword.New(username,password.getBytes()));
	}


	@Override
	public CredentialsUsernamePassword authenticate(final CredentialsUsernamePassword credentials)
			throws AuthenticationFailedException
	{
		return checkCredentials(credentials);
	}


	protected CredentialsUsernamePassword checkCredentials(
			final CredentialsUsernamePassword credentials) throws AuthenticationFailedException
	{
		final byte[] hashedPassword = this.passwordHasher.hashPassword(credentials.password());

		final CredentialsUsernamePassword found = Jpa
				.getDaoByEntityType(this.authenticationEntityType).findAll().stream()
				.filter(entity -> entity.username().equals(credentials.username())
						&& Arrays.equals(hashedPassword,entity.password()))
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

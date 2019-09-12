/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.security.authentication.ldap;

import javax.naming.NamingException;
import javax.naming.directory.DirContext;

import com.rapidclipse.framework.security.authentication.AuthenticationFailedException;
import com.rapidclipse.framework.security.authentication.Authenticator;
import com.rapidclipse.framework.security.authentication.CredentialsUsernamePassword;


/**
 * @author XDEV Software
 */

public class LDAPAuthenticator implements Authenticator<CredentialsUsernamePassword, DirContext>
{
	// /////////////////////////////////////////////////////////////////////////
	// instance fields //
	// //////////////////
	
	private final LDAPConfiguration configuration;
	
	// /////////////////////////////////////////////////////////////////////////
	// constructors //
	// ///////////////
	
	public LDAPAuthenticator(final LDAPConfiguration configuration)
	{
		super();
		
		this.configuration = configuration;
	}
	
	// /////////////////////////////////////////////////////////////////////////
	// declared methods //
	// ///////////////////
	
	public final DirContext authenticate(final String username, final String password)
		throws AuthenticationFailedException
	{
		return this.authenticate(CredentialsUsernamePassword.New(username, password));
	}
	
	// /////////////////////////////////////////////////////////////////////////
	// override methods //
	// ///////////////////
	
	@Override
	public DirContext authenticate(final CredentialsUsernamePassword credentials)
		throws AuthenticationFailedException
	{
		try(LDAPRealm realm = new LDAPRealm(this.configuration, credentials))
		{
			return realm.getLdapContext();
		}
		catch(final NamingException e)
		{
			throw new AuthenticationFailedException(e);
		}
	}
}

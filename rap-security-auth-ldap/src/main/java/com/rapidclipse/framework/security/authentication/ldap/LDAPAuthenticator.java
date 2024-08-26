/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

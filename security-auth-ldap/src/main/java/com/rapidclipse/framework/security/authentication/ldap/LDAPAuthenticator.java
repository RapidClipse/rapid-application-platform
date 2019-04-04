/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
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

/*-
 * ---
 * Rapid Application Platform / Security / Authentication and Authorization / LDAP
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

package com.rapidclipse.framework.security.authorization.ldap;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.naming.NamingException;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.mgt.DefaultSecurityManager;

import com.rapidclipse.framework.security.authentication.CredentialsUsernamePassword;
import com.rapidclipse.framework.security.authentication.ldap.LDAPConfiguration;
import com.rapidclipse.framework.security.authentication.ldap.LDAPRealm;
import com.rapidclipse.framework.security.authorization.AuthorizationConfiguration;
import com.rapidclipse.framework.security.authorization.AuthorizationConfigurationProvider;
import com.rapidclipse.framework.security.authorization.AuthorizationException;


/**
 *
 * @author XDEV Software (CK)
 */
public class LDAPAuthorizationConfigurationProvider implements AuthorizationConfigurationProvider
{
	private final LDAPConfiguration           configuration;
	private final CredentialsUsernamePassword credentials;
	
	public LDAPAuthorizationConfigurationProvider(
		final LDAPConfiguration configuration,
		final CredentialsUsernamePassword credentials)
	{
		this.configuration = configuration;
		this.credentials   = credentials;
	}
	
	/**
	 * @return the configuration
	 */
	public LDAPConfiguration getConfiguration()
	{
		return this.configuration;
	}
	
	/**
	 * @return the credentials
	 */
	public CredentialsUsernamePassword getCredentials()
	{
		return this.credentials;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public AuthorizationConfiguration provideConfiguration()
	{
		try(final LDAPRealm realm = new LDAPRealm(this.configuration, this.credentials))
		{
			SecurityUtils.setSecurityManager(new DefaultSecurityManager(realm));
			
			final Map<String, Set<String>>                  resourceResources = new HashMap<String, Set<String>>();
			final Map<String, Set<String>>                  roleRoles         = new HashMap<String, Set<String>>();
			final HashMap<String, HashMap<String, Integer>> rolePermissions   =
				new HashMap<String, HashMap<String, Integer>>();
			final Map<String, Set<String>>                  subjectRoles      = new HashMap<String, Set<String>>();
			
			final String                                    username          = this.credentials.username();
			final Set<String>                               groupNames        = realm.getGroupNamesForUser(username);
			subjectRoles.put(username, groupNames);
			
			for(final String group : groupNames)
			{
				final HashMap<String, Integer> permissions = new HashMap<String, Integer>();
				permissions.put(group, 1);
				rolePermissions.put(group, permissions);
				roleRoles.put(group, new HashSet<String>());
				resourceResources.put(group, new HashSet<String>());
			}
			
			return AuthorizationConfiguration.New(resourceResources, roleRoles, rolePermissions,
				subjectRoles);
		}
		catch(final NamingException e)
		{
			throw new AuthorizationException(e);
		}
	}
}

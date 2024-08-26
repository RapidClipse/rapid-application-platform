/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.security.authentication.ldap;

/**
 *
 * @author XDEV Software
 */
public class LDAPConfiguration
{
	public static class LDAPConfigurationBuilder
	{
		private final String providerUrl;
		
		// Optional
		private String suffix = "";
		private String searchbase;
		private String securityAuthentication;
		private String securityProtocol;
		
		public LDAPConfigurationBuilder(final String providerUrl)
		{
			this.providerUrl = providerUrl;
		}
		
		public LDAPConfigurationBuilder suffix(final String suffix)
		{
			this.suffix = suffix;
			return this;
		}
		
		public LDAPConfigurationBuilder searchBase(final String searchBase)
		{
			this.searchbase = searchBase;
			return this;
		}
		
		public LDAPConfigurationBuilder securityAuthentication(final String securityAuthentication)
		{
			this.securityAuthentication = securityAuthentication;
			return this;
		}
		
		public LDAPConfigurationBuilder securityProtocol(final String securityProtocol)
		{
			this.securityProtocol = securityProtocol;
			return this;
		}
		
		public LDAPConfiguration build()
		{
			return new LDAPConfiguration(this);
		}
	}
	
	private final String providerUrl;
	
	// Optional
	private final String suffix;
	private final String searchbase;
	private final String securityAuthentication;
	private final String securityProtocol;
	
	/**
	 *
	 */
	private LDAPConfiguration(final LDAPConfigurationBuilder builder)
	{
		this.providerUrl = builder.providerUrl;
		
		// optionals
		this.suffix                 = builder.suffix;
		this.searchbase             = builder.searchbase;
		this.securityAuthentication = builder.securityAuthentication;
		this.securityProtocol       = builder.securityProtocol;
	}
	
	public String getSuffix()
	{
		return this.suffix;
	}
	
	public String getProviderUrl()
	{
		return this.providerUrl;
	}
	
	public String getSearchbase()
	{
		return this.searchbase;
	}
	
	public String getSecurityAuthentication()
	{
		return this.securityAuthentication;
	}
	
	public String getSecurityProtocol()
	{
		return this.securityProtocol;
	}
}

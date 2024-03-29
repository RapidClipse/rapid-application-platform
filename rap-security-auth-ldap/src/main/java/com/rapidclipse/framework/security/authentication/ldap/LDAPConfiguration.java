/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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

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

package com.rapidclipse.framework.server.mobilekit.contacts;

import org.apache.commons.lang3.StringUtils;


/**
 * Contains different kinds of information about a Contact object's name.
 *
 * @author XDEV Software
 *
 */
public class ContactName
{
	private String formatted;
	private String familyName;
	private String givenName;
	private String middle;
	private String prefix;
	private String suffix;
	
	public ContactName()
	{
	}
	
	public ContactName(
		final String formatted,
		final String familyName,
		final String givenName,
		final String middle,
		final String prefix,
		final String suffix)
	{
		this.formatted  = formatted;
		this.familyName = familyName;
		this.givenName  = givenName;
		this.middle     = middle;
		this.prefix     = prefix;
		this.suffix     = suffix;
	}
	
	/**
	 * The complete name of the contact.
	 */
	public String getFormatted()
	{
		return this.formatted;
	}
	
	/**
	 * The complete name of the contact.
	 */
	public ContactName setFormatted(final String formatted)
	{
		this.formatted = formatted;
		return this;
	}
	
	/**
	 * The contact's family name.
	 */
	public String getFamilyName()
	{
		return this.familyName;
	}
	
	/**
	 * The contact's family name.
	 */
	public ContactName setFamilyName(final String familyName)
	{
		this.familyName = familyName;
		return this;
	}
	
	/**
	 * The contact's given name.
	 */
	public String getGivenName()
	{
		return this.givenName;
	}
	
	/**
	 * The contact's given name.
	 */
	public ContactName setGivenName(final String givenName)
	{
		this.givenName = givenName;
		return this;
	}
	
	/**
	 * The contact's middle name.
	 */
	public String getMiddle()
	{
		return this.middle;
	}
	
	/**
	 * The contact's middle name.
	 */
	public ContactName setMiddle(final String middle)
	{
		this.middle = middle;
		return this;
	}
	
	/**
	 * The contact's prefix (example Mr. or Dr.)
	 */
	public String getPrefix()
	{
		return this.prefix;
	}
	
	/**
	 * The contact's prefix (example Mr. or Dr.)
	 */
	public ContactName setPrefix(final String prefix)
	{
		this.prefix = prefix;
		return this;
	}
	
	/**
	 * The contact's suffix (example Esq.)
	 */
	public String getSuffix()
	{
		return this.suffix;
	}
	
	/**
	 * The contact's suffix (example Esq.)
	 */
	public ContactName setSuffix(final String suffix)
	{
		this.suffix = suffix;
		return this;
	}
	
	@Override
	public String toString()
	{
		if(StringUtils.isNotBlank(this.formatted))
		{
			return this.formatted;
		}
		
		if(StringUtils.isNotBlank(this.givenName) || StringUtils.isNotBlank(this.familyName))
		{
			final StringBuilder sb = new StringBuilder();
			if(StringUtils.isNotBlank(this.prefix))
			{
				sb.append(this.prefix).append(' ');
			}
			if(StringUtils.isNotBlank(this.givenName))
			{
				sb.append(this.givenName).append(' ');
			}
			if(StringUtils.isNotBlank(this.middle))
			{
				sb.append(this.middle).append(' ');
			}
			if(StringUtils.isNotBlank(this.familyName))
			{
				sb.append(this.familyName).append(' ');
			}
			if(StringUtils.isNotBlank(this.suffix))
			{
				sb.append(this.suffix);
			}
			return sb.toString().trim();
		}
		
		return super.toString();
	}
	
	boolean hasToStringContent()
	{
		return StringUtils.isNotBlank(this.formatted) || StringUtils.isNotBlank(this.givenName)
			|| StringUtils.isNotBlank(this.familyName);
	}
}

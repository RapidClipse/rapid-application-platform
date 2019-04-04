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

import java.io.Serializable;


/**
 * The ContactField object is a reusable component that represents contact
 * fields generically. Each ContactField object contains a value, type, and pref
 * property. A {@link Contact} object stores several properties in ContactField
 * lists, such as phone numbers and email addresses.
 * <p>
 * In most instances, there are no pre-determined values for a ContactField
 * object's type attribute. For example, a phone number can specify type values
 * of home, work, mobile, iPhone, or any other value that is supported by a
 * particular device platform's contact database. However, for the
 * {@link Contact} photos property, the type property indicates the format of
 * the returned image: url when the value attribute contains a URL to the photo
 * image, or base64 when the value contains a base64-encoded image string.
 *
 *
 * @author XDEV Software
 *
 */
public class ContactField implements Serializable
{
	private String  id;
	private String  type;
	private String  value;
	private boolean pref;
	
	public ContactField()
	{
	}
	
	public ContactField(final String id, final String type, final String value, final boolean pref)
	{
		this.id    = id;
		this.type  = type;
		this.value = value;
		this.pref  = pref;
	}
	
	/**
	 * Unique identifier
	 */
	public String getId()
	{
		return this.id;
	}
	
	/**
	 * Unique identifier
	 */
	public ContactField setId(final String id)
	{
		this.id = id;
		return this;
	}
	
	/**
	 * A string that indicates what type of field this is, home for example.
	 */
	public String getType()
	{
		return this.type;
	}
	
	/**
	 * A string that indicates what type of field this is, home for example.
	 */
	public ContactField setType(final String type)
	{
		this.type = type;
		return this;
	}
	
	/**
	 * The value of the field, such as a phone number or email address.
	 */
	public String getValue()
	{
		return this.value;
	}
	
	/**
	 * The value of the field, such as a phone number or email address.
	 */
	public ContactField setValue(final String value)
	{
		this.value = value;
		return this;
	}
	
	/**
	 * Set to <code>true</code> if this ContactField contains the user's
	 * preferred value.
	 */
	public boolean isPref()
	{
		return this.pref;
	}
	
	/**
	 * Set to <code>true</code> if this ContactField contains the user's
	 * preferred value.
	 */
	public ContactField setPref(final boolean pref)
	{
		this.pref = pref;
		return this;
	}
	
	@Override
	public String toString()
	{
		return this.type + " = " + this.value;
	}
}

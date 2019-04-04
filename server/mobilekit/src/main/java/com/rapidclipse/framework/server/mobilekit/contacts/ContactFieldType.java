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

/**
 * @author XDEV Software
 *
 */
public enum ContactFieldType
{
	ADDRESSES("addresses"),
	BIRTHDAY("birthday"),
	CATEGORIES("categories"),
	COUNTRY("country"),
	DEPARTMENT("department"),
	DISPLAY_NAME("displayName"),
	EMAILS("emails"),
	FAMILY_NAME("familyName"),
	FORMATTED("formatted"),
	GIVEN_NAME("givenName"),
	HONORIFIC_PREFIX("honorificPrefix"),
	HONORIFICS_UFFIX("honorificSuffix"),
	ID("id"),
	IMS("ims"),
	LOCALITY("locality"),
	MIDDLE_NAME("middleName"),
	NAME("name"),
	NICKNAME("nickname"),
	NOTE("note"),
	ORGANIZATIONS("organizations"),
	PHONE_NUMBERS("phoneNumbers"),
	PHOTOS("photos"),
	POSTAL_CODE("postalCode"),
	REGION("region"),
	STREET_ADDRESS("streetAddress"),
	TITLE("title"),
	URLS("urls");
	
	private String fieldName;
	
	private ContactFieldType(final String fieldName)
	{
		this.fieldName = fieldName;
	}
	
	public String getFieldName()
	{
		return this.fieldName;
	}
}

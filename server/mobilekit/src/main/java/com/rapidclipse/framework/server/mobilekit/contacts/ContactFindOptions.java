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
 * Search options to filter contacts.
 *
 * @author XDEV Software
 * 
 */
public class ContactFindOptions
{
	public static ContactFindOptions all()
	{
		return new ContactFindOptions().multipleResults();
	}
	
	public static ContactFindOptions byName(final String name)
	{
		return new ContactFindOptions().filter(name, ContactFieldType.DISPLAY_NAME,
			ContactFieldType.NAME, ContactFieldType.FAMILY_NAME, ContactFieldType.GIVEN_NAME)
			.multipleResults();
	}
	
	public static ContactFindOptions byPhoneNumber(final String number)
	{
		return new ContactFindOptions().filter(number, ContactFieldType.PHONE_NUMBERS)
			.multipleResults();
	}
	
	private String             filter              = "";
	private ContactFieldType[] filterFields;
	private boolean            multiple            = true;
	private boolean            mustHavePhoneNumber = false;
	private ContactFieldType[] desiredFields;
	
	public ContactFindOptions()
	{
	}
	
	/**
	 *
	 * @param filter
	 *            The search string used to find contacts
	 * @param filterFields
	 *            [optional] Fields to search in
	 * @return
	 */
	public ContactFindOptions filter(final String filter, final ContactFieldType... filterFields)
	{
		this.filter       = filter;
		this.filterFields = filterFields;
		return this;
	}
	
	/**
	 * The search only returns the first result.
	 *
	 * @see #multipleResults()
	 */
	public ContactFindOptions singleResult()
	{
		this.multiple = false;
		return this;
	}
	
	/**
	 * The search returns all results.
	 *
	 * @see #singleResult()
	 */
	public ContactFindOptions multipleResults()
	{
		this.multiple = true;
		return this;
	}
	
	/**
	 * Filters the search to only return contacts with a phone number informed.
	 * (Android only)
	 */
	public ContactFindOptions mustHavePhoneNumer()
	{
		this.mustHavePhoneNumber = true;
		return this;
	}
	
	/**
	 * Limit contact fields to be returned back.
	 */
	public ContactFindOptions returnFields(final ContactFieldType... fieldTypes)
	{
		this.desiredFields = fieldTypes;
		return this;
	}
	
	public String getFilter()
	{
		return this.filter;
	}
	
	public ContactFieldType[] getFilterFields()
	{
		return this.filterFields;
	}
	
	public boolean isMultiple()
	{
		return this.multiple;
	}
	
	public boolean isMustHavePhoneNumber()
	{
		return this.mustHavePhoneNumber;
	}
	
	public ContactFieldType[] getDesiredFields()
	{
		return this.desiredFields;
	}
}

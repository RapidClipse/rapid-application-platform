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

import java.util.List;
import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * Service which provides access to the device contacts database.
 * <p>
 * <b>Warning:</b><br>
 * Collection and use of contact data raises important privacy issues. Your
 * app's privacy policy should discuss how the app uses contact data and whether
 * it is shared with any other parties. Contact information is considered
 * sensitive because it reveals the people with whom a person communicates.
 * Therefore, in addition to the app's privacy policy, you should strongly
 * consider providing a just-in-time notice before the app accesses or uses
 * contact data, if the device operating system doesn't do so already. That
 * notice should provide the same information noted above, as well as obtaining
 * the user's permission (e.g., by presenting choices for OK and No Thanks).
 * Note that some app marketplaces may require the app to provide a just-in-time
 * notice and obtain the user's permission before accessing contact data. A
 * clear and easy-to-understand user experience surrounding the use of contact
 * data helps avoid user confusion and perceived misuse of contact data. For
 * more information, please see the <a href=
 * "http://cordova.apache.org/docs/en/latest/guide/appdev/privacy/index.html">
 * Privacy Guide</a>.
 *
 *
 * @author XDEV Software
 *
 */
@ServiceComponent(ContactsComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-contacts", spec = "3.0.1"))
public interface ContactsService extends MobileService
{
	public static ContactsService getCurrent()
	{
		return MobileService.getCurrent(ContactsService.class);
	}
	
	/**
	 * Finds contacts in the device contacts database.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows Phone 8</li>
	 * <li>Windows</li>
	 * </ul>
	 */
	default public void findContacts(
		final ContactFindOptions options,
		final Consumer<List<Contact>> successCallback)
	{
		findContacts(options, successCallback, null);
	}
	
	/**
	 * Finds contacts in the device contacts database.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows Phone 8</li>
	 * <li>Windows</li>
	 * </ul>
	 */
	public void findContacts(
		ContactFindOptions options,
		Consumer<List<Contact>> successCallback,
		Consumer<ContactsServiceError> errorCallback);
	
	/**
	 * Launches the Contact Picker to select a single contact.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows Phone 8</li>
	 * <li>Windows</li>
	 * </ul>
	 */
	default public void pickContact(final Consumer<Contact> successCallback)
	{
		pickContact(successCallback, null);
	}
	
	/**
	 * Launches the Contact Picker to select a single contact.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows Phone 8</li>
	 * <li>Windows</li>
	 * </ul>
	 */
	public void pickContact(
		Consumer<Contact> successCallback,
		Consumer<ContactsServiceError> errorCallback);
	
	/**
	 * Saves a new contact to the device contacts database, or updates an
	 * existing contact if a contact with the same id already exists.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows Phone 8</li>
	 * <li>Windows</li>
	 * </ul>
	 */
	default public void saveContact(final Contact contact, final Consumer<Contact> successCallback)
	{
		saveContact(contact, successCallback, null);
	}
	
	/**
	 * Saves a new contact to the device contacts database, or updates an
	 * existing contact if a contact with the same id already exists.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows Phone 8</li>
	 * <li>Windows</li>
	 * </ul>
	 */
	public void saveContact(
		Contact contact,
		Consumer<Contact> successCallback,
		Consumer<ContactsServiceError> errorCallback);
}

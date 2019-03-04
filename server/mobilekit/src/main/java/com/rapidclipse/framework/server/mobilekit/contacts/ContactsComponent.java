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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.rapidclipse.framework.server.mobilekit.contacts.ContactsServiceError.Reason;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;

import elemental.json.JsonArray;
import elemental.json.JsonObject;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-contacts")
@HtmlImport("contacts.html")
public class ContactsComponent extends MobileComponent implements ContactsService
{
	public ContactsComponent()
	{
		super();
	}
	
	@Override
	public void findContacts(
		final ContactFindOptions options,
		final Consumer<List<Contact>> successCallback,
		final Consumer<ContactsServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("findContacts", id, generateFieldsJS(options), generateOptionsJS(options));
	}
	
	private String generateFieldsJS(final ContactFindOptions options)
	{
		final StringBuilder js = new StringBuilder();
		js.append("[");
		final ContactFieldType[] filterFields = options.getFilterFields();
		if(filterFields == null || filterFields.length == 0)
		{
			js.append("'*'");
		}
		else
		{
			js.append(Arrays.stream(filterFields).map(f -> "navigator.contacts.fieldType." + f.getFieldName())
				.collect(Collectors.joining(",")));
		}
		js.append("];");
		return js.toString();
	}
	
	private String generateOptionsJS(final ContactFindOptions options)
	{
		final StringBuilder js = new StringBuilder();
		js.append("{");
		js.append("filter:'").append(options.getFilter().replace("'", "\\'"))
			.append("',");
		js.append("multiple:").append(options.isMultiple()).append(",");
		js.append("hasPhoneNumber:").append(options.isMustHavePhoneNumber());
		final ContactFieldType[] desiredFields = options.getDesiredFields();
		if(desiredFields != null && desiredFields.length > 0)
		{
			js.append(",desiredFields:[");
			js.append(Arrays.stream(desiredFields).map(f -> "navigator.contacts.fieldType." + f.getFieldName())
				.collect(Collectors.joining(",")));
			js.append("]");
		}
		js.append("}");
		return js.toString();
	}
	
	@ClientCallable
	void findContacts_success(final String id, final JsonArray contactsArray)
	{
		final List<Contact> contacts = new ArrayList<>();
		for(int i = 0, c = contactsArray.length(); i < c; i++)
		{
			contacts.add(toJava(contactsArray.getObject(i), Contact.class));
		}
		getAndRemoveCall(id).success(contacts);
	}
	
	@ClientCallable
	void findContacts_error(final String id, final String errorMessage)
	{
		final ContactsServiceError error = createContactsServiceError(errorMessage);
		getAndRemoveCall(id).error(error);
	}
	
	@Override
	public void pickContact(final Consumer<Contact> successCallback, final Consumer<ContactsServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("pickContact", id);
	}
	
	@ClientCallable
	void pickContact_success(
		final String id,
		final JsonObject contactObj,
		final String watchId)
	{
		final Contact contact = toJava(contactObj, Contact.class);
		getAndRemoveCall(id).success(contact);
	}
	
	@ClientCallable
	void pickContact_error(final String id, final String errorMessage)
	{
		final ContactsServiceError error = createContactsServiceError(errorMessage);
		getAndRemoveCall(id).error(error);
	}
	
	@Override
	public void saveContact(
		final Contact contact,
		final Consumer<Contact> successCallback,
		final Consumer<ContactsServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("saveContact", id, toJson(contact));
	}
	
	@ClientCallable
	void saveContact_success(
		final String id,
		final JsonObject contactObj,
		final String watchId)
	{
		final Contact contact = toJava(contactObj, Contact.class);
		getAndRemoveCall(id).success(contact);
	}
	
	@ClientCallable
	void saveContact_error(final String id, final String errorMessage)
	{
		final ContactsServiceError error = createContactsServiceError(errorMessage);
		getAndRemoveCall(id).error(error);
	}
	
	private ContactsServiceError createContactsServiceError(final String message)
	{
		Reason reason = null;
		
		try
		{
			final int code = Integer.parseInt(message);
			reason = Reason.getByCode(code);
		}
		catch(final Exception e)
		{
			// swallow
		}
		
		return new ContactsServiceError(this, message, reason);
	}
}

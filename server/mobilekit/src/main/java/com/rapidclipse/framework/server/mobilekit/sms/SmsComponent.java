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
package com.rapidclipse.framework.server.mobilekit.sms;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-sms")
@HtmlImport("sms.html")
public class SmsComponent extends MobileComponent implements SmsService
{
	public SmsComponent()
	{
		super();
	}
	
	@Override
	public void sendSms(
		final String number,
		final String message,
		final SmsOptions options,
		final Runnable successCallback,
		final Consumer<MobileServiceError> errorCallback)
	{
		final Consumer<Object> successWrapper = successCallback != null
			? object -> successCallback.run() : null;
		final String           id             = registerCall(successWrapper, errorCallback);
		getElement().callFunction("sendSms", id, number, message, toJson(options));
	}
	
	private String toJson(final SmsOptions options)
	{
		final StringBuilder sb = new StringBuilder();
		sb.append("{replaceLineBreaks:").append(options.isReplaceLineBreaks()).append(",android:{")
			.append("intent:").append(options.isAndroidNativeApp() ? "'INTENT'" : "''")
			.append("}}");
		return sb.toString();
	}
	
	@ClientCallable
	void sendSms_success(final String id)
	{
		getAndRemoveCall(id).success(null);
	}
	
	@ClientCallable
	void sendSms_error(final String id, final String errorMessage)
	{
		final MobileServiceError error = new MobileServiceError(this, errorMessage);
		getAndRemoveCall(id).error(error);
	}
}

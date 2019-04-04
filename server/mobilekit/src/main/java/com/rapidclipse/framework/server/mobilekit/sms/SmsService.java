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

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * Service to easily send SMS.
 *
 * @author XDEV Software
 *
 */
@ServiceComponent(SmsComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-sms-plugin", spec = "1.0.0"))
public interface SmsService extends MobileService
{
	public static SmsService getCurrent()
	{
		return MobileService.getCurrent(SmsService.class);
	}
	
	/**
	 * Sends a SMS to the specified number.
	 *
	 * @param successCallback
	 *            The function to call when the SMS was sent successfully
	 * @param errorCallback
	 *            The function to call when there is an error sending the SMS.
	 */
	public void sendSms(
		String number,
		String message,
		SmsOptions options,
		Runnable successCallback,
		Consumer<MobileServiceError> errorCallback);
}

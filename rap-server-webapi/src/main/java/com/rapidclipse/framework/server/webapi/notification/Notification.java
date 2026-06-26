/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.notification;

import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.UI;


/**
 * Simple static interface that lets you send local notifications to the current device. These notifications will be
 * shown as a small popup that can contain images and vibration patterns.<br>
 * <b>Note:</b> The device has to
 * first give permission to receive these notifications.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public final class Notification
{
	/**
	 * Send a local notification to the device. First asks for permission and if permission is granted shows the local
	 * notification.
	 *
	 * @param title
	 *            The title of the notification.
	 * @param options
	 *            Various options for the notification. Ranges from images and descriptions to vibration patterns.
	 */
	public static void showLocalNotification(final String title, final LocalNotificationOptions options)
	{
		UI.getCurrent()
			.getPage()
			.executeJs(
				"Notification.requestPermission().then(res => {" +
					"if(res==='granted') navigator.serviceWorker.ready.then(reg=> reg.showNotification($0,$1));" +
					"})",
				title,
				JsonUtils.encodeObject(options));
	}

	private Notification()
	{
		throw new Error();
	}
}

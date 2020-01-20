/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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

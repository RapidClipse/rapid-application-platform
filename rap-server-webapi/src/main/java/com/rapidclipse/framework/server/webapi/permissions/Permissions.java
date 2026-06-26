/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.permissions;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.function.SerializableConsumer;


/**
 * Utility class used for permission related queries. You can query the current device permissions with the
 * {@link #getPermissionStatus(PermissionType, SerializableConsumer)} method.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public final class Permissions
{
	// TODO Method for asking for permissions (Currently not supported by any browser)
	
	/**
	 * Query the current permission status for a given permission.
	 *
	 * @param permission
	 *            The permission that should be queried.
	 * @param callback
	 *            The callback that is triggered when the permission status is received. It will consume the received
	 *            permission status.
	 */
	public static void getPermissionStatus(
		final PermissionType permission,
		final SerializableConsumer<PermissionStatus> callback)
	{
		UI.getCurrent()
			.getPage()
			.executeJs("return (async () => (await navigator.permissions.query({name: $0})).state )()",
				permission.toString())
			.then(String.class, s -> callback.accept(PermissionStatus.valueOf(s)));
	}
	
	private Permissions()
	{
		throw new Error();
	}
}

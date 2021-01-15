/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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

/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.webapi.permissions;

/**
 * Enumeration of all the permissions that are currently available.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public enum PermissionType
{
	geolocation,
	notifications,
	push,
	midi,
	camera,
	microphone,
	speaker,
	device_info,
	background_fetch,
	background_sync,
	bluetooth,
	persistent_storage,
	ambient_light_sensor,
	accelerometer,
	gyroscope,
	magnetometer,
	clipboard,
	display_catpure,
	nfc;
	
	public static PermissionType fromString(final String str)
	{
		return PermissionType.valueOf(str.replace('-', '_'));
	}
	
	@Override
	public String toString()
	{
		return this.name().replace('_', '-');
	}
}

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
package com.rapidclipse.framework.server.webapi.geolocation;

import java.io.Serializable;


/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PositionError implements Serializable
{
	/**
	 * From MDN:<br>
	 * <b>1 PERMISSION_DENIED</b> The acquisition of the geolocation information failed because the page didn't have the
	 * permission to do it.<br>
	 * <b>2 POSITION_UNAVAILABLE</b> The acquisition of the geolocation failed because at least one internal source of
	 * position
	 * returned an internal error.<br>
	 * <b>3 TIMEOUT</b> The time allowed to acquire the geolocation, defined by PositionOptions.timeout information was
	 * reached
	 * before the information was obtained.
	 */
	private short  code;
	private String message;

	public PositionError()
	{
		super();
	}

	public short getCode()
	{
		return this.code;
	}

	public PositionError setCode(final short code)
	{
		this.code = code;
		return this;
	}

	public String getMessage()
	{
		return this.message;
	}

	public PositionError setMessage(final String message)
	{
		this.message = message;
		return this;
	}
}

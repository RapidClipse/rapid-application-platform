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
package com.rapidclipse.framework.server.webapi.geolocation;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public class Position implements Serializable
{
	private Coordinates coords;
	private long        timestamp;
	
	public Position()
	{
	}
	
	public Position(final Coordinates coords, final long timestamp)
	{
		this.coords    = coords;
		this.timestamp = timestamp;
	}
	
	public Coordinates getCoords()
	{
		return this.coords;
	}
	
	public Position setCoords(final Coordinates coords)
	{
		this.coords = coords;
		return this;
	}
	
	public long getTimestamp()
	{
		return this.timestamp;
	}
	
	public Position setTimestamp(final long timestamp)
	{
		this.timestamp = timestamp;
		return this;
	}
}

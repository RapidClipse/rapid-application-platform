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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
 * @author XDEV Software
 * @since 10.02.00
 */
public class Coordinates implements Serializable
{
	/* These values are required to be set */
	private final double latitude;
	private final double longitude;
	private final double accuracy;

	/* These values are not required to be set and therefore can be null */
	private final Double altitude;
	private final Double altitudeAccuracy;
	private final Double heading;
	private final Double speed;

	public Coordinates(
		final double latitude,
		final double longitude,
		final Double altitude,
		final double accuracy,
		final Double altitudeAccuracy,
		final Double heading,
		final Double speed)
	{
		this.latitude         = latitude;
		this.longitude        = longitude;
		this.altitude         = altitude;
		this.accuracy         = accuracy;
		this.altitudeAccuracy = altitudeAccuracy;
		this.heading          = heading;
		this.speed            = speed;
	}

	public double getLatitude()
	{
		return this.latitude;
	}

	public double getLongitude()
	{
		return this.longitude;
	}

	public Double getAltitude()
	{
		return this.altitude;
	}

	public double getAccuracy()
	{
		return this.accuracy;
	}

	public Double getAltitudeAccuracy()
	{
		return this.altitudeAccuracy;
	}

	public Double getHeading()
	{
		return this.heading;
	}

	public Double getSpeed()
	{
		return this.speed;
	}
}

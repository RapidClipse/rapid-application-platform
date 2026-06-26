/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.mobilekit.geolocation;


/**
 * A Coordinates object is attached to a {@link Position} object that is
 * available to callback functions in requests for the current position. It
 * contains a set of properties that describe the geographic coordinates of a
 * position.
 *
 * @author XDEV Software
 *
 */
public interface Coordinates
{
	/**
	 * Latitude in decimal degrees.
	 */
	public double getLatitude();


	/**
	 * Longitude in decimal degrees.
	 */
	public double getLongitude();


	/**
	 * Height of the position in meters above the ellipsoid.
	 */
	public double getAltitude();


	/**
	 * Accuracy level of the latitude and longitude coordinates in meters.
	 */
	public double getAccuracy();


	/**
	 * Accuracy level of the altitude coordinate in meters.
	 */
	public double getAltitudeAccuracy();


	/**
	 * Direction of travel, specified in degrees counting clockwise relative to
	 * the true north.
	 */
	public double getHeading();


	/**
	 * Current ground speed of the device, specified in meters per second.
	 */
	public double getSpeed();
}

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

package com.rapidclipse.framework.server.mobilekit.accelerometer;


/**
 * Contains Accelerometer data captured at a specific point in time.
 * Acceleration values include the effect of gravity (9.81 m/s^2), so that when
 * a device lies flat and facing up, x, y, and z values returned should be 0, 0,
 * and 9.81.
 *
 * @author XDEV Software
 *
 */
public interface Acceleration
{
	/**
	 * Amount of acceleration on the x-axis. (in m/s^2)
	 */
	public double getX();
	
	
	/**
	 * Amount of acceleration on the y-axis. (in m/s^2)
	 */
	public double getY();
	
	
	/**
	 * Amount of acceleration on the z-axis. (in m/s^2)
	 */
	public double getZ();
	
	
	/**
	 * Creation timestamp in milliseconds.
	 */
	public long getTimestamp();
}

/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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

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
 * Parameters to customize the retrieval of the acceleration.
 *
 * @author XDEV Software
 *
 */
public class AccelerometerOptions
{
	public static AccelerometerOptions withFrequency(final int milliseconds)
	{
		return new AccelerometerOptions().frequency(milliseconds);
	}
	
	private int frequency = 1000;
	
	public AccelerometerOptions()
	{
	}
	
	/**
	 * The frequency of updates in milliseconds.
	 */
	public int getFrequency()
	{
		return this.frequency;
	}
	
	/**
	 * The frequency of updates in milliseconds.
	 */
	public AccelerometerOptions frequency(final int frequency)
	{
		this.frequency = frequency;
		return this;
	}
}

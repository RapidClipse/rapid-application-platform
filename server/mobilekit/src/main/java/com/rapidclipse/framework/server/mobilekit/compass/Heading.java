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

package com.rapidclipse.framework.server.mobilekit.compass;

/**
 * Contains heading data and timestamp created by the {@link CompassService}.
 *
 * @author XDEV Software
 *
 */
public interface Heading
{
	/**
	 * The heading in degrees from 0-359.99 at a single moment in time.
	 */
	public double getMagneticHeading();
	
	/**
	 * The heading relative to the geographic North Pole in degrees 0-359.99 at
	 * a single moment in time. A negative value indicates that the true heading
	 * can't be determined.
	 */
	public double getTrueHeading();
	
	/**
	 * The deviation in degrees between the reported heading and the true
	 * heading.
	 */
	public double getHeadingAccuracy();
	
	/**
	 * The time at which this heading was determined.
	 */
	public long getTimestamp();
}

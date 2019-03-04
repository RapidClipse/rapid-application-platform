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

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * This service provides access to the device's compass. The compass is a sensor
 * that detects the direction or heading that the device is pointed, typically
 * from the top of the device. It measures the heading in degrees from 0 to
 * 359.99, where 0 is north.
 *
 * @author XDEV Software
 *
 */
@ServiceComponent(CompassComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-device-orientation", spec = "2.0.1"))
public interface CompassService extends MobileService
{
	public static CompassService getCurrent()
	{
		return MobileService.getCurrent(CompassService.class);
	}
	
	/**
	 * Asynchronously acquires the current heading.
	 *
	 * @param successCallback
	 *            The function to call when the heading data is available
	 */
	default public void getCurrentHeading(
		final CompassOptions options,
		final Consumer<Heading> successCallback)
	{
		getCurrentHeading(options, successCallback, null);
	}
	
	/**
	 * Asynchronously acquires the current heading.
	 *
	 * @param successCallback
	 *            The function to call when the heading data is available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            heading position.
	 */
	public void getCurrentHeading(
		CompassOptions options,
		Consumer<Heading> successCallback,
		Consumer<MobileServiceError> errorCallback);
	
	/**
	 * Asynchronously watches the compass for changes to heading. When a change
	 * occurs, the successCallback is called with the new location.
	 *
	 * @param options
	 *            optional options
	 * @param successCallback
	 *            The function to call each time the heading data is available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            heading data.
	 */
	default public void watchHeading(
		final CompassOptions options,
		final Consumer<HeadingWatch> successCallback)
	{
		watchHeading(options, successCallback, null);
	}
	
	/**
	 * Asynchronously watches the compass for changes to heading. When a change
	 * occurs, the successCallback is called with the new location.
	 *
	 * @param options
	 *            optional options
	 * @param successCallback
	 *            The function to call each time the heading data is available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            heading data.
	 */
	public void watchHeading(
		CompassOptions options,
		Consumer<HeadingWatch> successCallback,
		Consumer<MobileServiceError> errorCallback);
}

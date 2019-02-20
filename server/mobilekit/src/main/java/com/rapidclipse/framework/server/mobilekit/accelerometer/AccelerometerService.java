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

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * @author XDEV Software
 *
 */
@ServiceComponent(AccelerometerComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-device-motion", spec = "2.0.1"))
public interface AccelerometerService extends MobileService
{
	public static AccelerometerService getCurrent()
	{
		return MobileService.getCurrent(AccelerometerService.class);
	}
	
	/**
	 * Get the current acceleration along the x, y, and z axes.
	 *
	 * These acceleration values are returned to the success callback function.
	 *
	 * @param successCallback
	 *            The function to call when the acceleration data is available
	 */
	default public void getCurrentAcceleration(
		final AccelerometerOptions options,
		final Consumer<Acceleration> successCallback)
	{
		getCurrentAcceleration(options, successCallback, null);
	}
	
	/**
	 * Get the current acceleration along the x, y, and z axes.
	 *
	 * These acceleration values are returned to the success callback function.
	 *
	 * @param successCallback
	 *            The function to call when the acceleration data is available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            acceleration data.
	 */
	public void getCurrentAcceleration(
		AccelerometerOptions options,
		Consumer<Acceleration> successCallback,
		Consumer<MobileServiceError> errorCallback);
	
	/**
	 * Retrieves the device's current acceleration at a regular interval,
	 * executing the success callback each time. Specify the interval in
	 * milliseconds via the option's frequency parameter.
	 *
	 * @param options
	 * @param successCallback
	 *            The function to call each time the acceleration data is
	 *            available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            acceleration data.
	 */
	default public void watchAcceleration(
		final AccelerometerOptions options,
		final Consumer<AccelerationWatch> successCallback)
	{
		watchAcceleration(options, successCallback, null);
	}
	
	/**
	 * Retrieves the device's current acceleration at a regular interval,
	 * executing the success callback each time. Specify the interval in
	 * milliseconds via the option's frequency parameter.
	 *
	 * @param options
	 * @param successCallback
	 *            The function to call each time the acceleration data is
	 *            available
	 * @param errorCallback
	 *            The function to call when there is an error getting the
	 *            acceleration data.
	 */
	public void watchAcceleration(
		AccelerometerOptions options,
		Consumer<AccelerationWatch> successCallback,
		Consumer<MobileServiceError> errorCallback);
}

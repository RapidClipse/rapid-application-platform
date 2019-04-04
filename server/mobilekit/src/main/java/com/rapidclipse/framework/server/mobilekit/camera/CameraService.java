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
package com.rapidclipse.framework.server.mobilekit.camera;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * Service for taking pictures and for choosing images from the system's image
 * library.
 *
 * @author XDEV Software
 *
 */
@ServiceComponent(CameraComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-camera", spec = "4.0.3"))
public interface CameraService extends MobileService
{
	public static CameraService getCurrent()
	{
		return MobileService.getCurrent(CameraService.class);
	}
	
	/**
	 * Takes a photo using the camera, or retrieves a photo from the device's
	 * image gallery. The image is passed to the success callback as a
	 * base64-encoded String, or as the URI for the image file.
	 * <p>
	 * Photo resolution on newer devices is quite good. Photos selected from the
	 * device's gallery are not downscaled to a lower quality, even if a quality
	 * parameter is specified. To avoid common memory problems, set
	 * destinationType to {@link DestinationType#FILE_URI} rather than
	 * {@link DestinationType#IMAGE}.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows</li>
	 * <li>Windows Phone 8</li>
	 * </ul>
	 *
	 */
	default public void getPicture(
		final CameraOptions options,
		final Consumer<Picture> successCallback)
	{
		getPicture(options, successCallback, null);
	}
	
	/**
	 * Takes a photo using the camera, or retrieves a photo from the device's
	 * image gallery. The image is passed to the success callback as a
	 * base64-encoded String, or as the URI for the image file.
	 * <p>
	 * Photo resolution on newer devices is quite good. Photos selected from the
	 * device's gallery are not downscaled to a lower quality, even if a quality
	 * parameter is specified. To avoid common memory problems, set
	 * destinationType to {@link DestinationType#FILE_URI} rather than
	 * {@link DestinationType#IMAGE}.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows</li>
	 * <li>Windows Phone 8</li>
	 * </ul>
	 *
	 */
	public void getPicture(
		CameraOptions options,
		Consumer<Picture> successCallback,
		Consumer<MobileServiceError> errorCallback);
}

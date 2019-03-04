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

package com.rapidclipse.framework.server.mobilekit.deviceinfo;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * @author XDEV Software
 *
 */
@ServiceComponent(DeviceInfoComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-device", spec = "2.0.2"))
public interface DeviceInfoService extends MobileService
{
	public static DeviceInfoService getCurrent()
	{
		return MobileService.getCurrent(DeviceInfoService.class);
	}
	
	/**
	 * Queries the device's hardware and software info.
	 */
	public void getDeviceInfo(Consumer<DeviceInfo> callback);
}

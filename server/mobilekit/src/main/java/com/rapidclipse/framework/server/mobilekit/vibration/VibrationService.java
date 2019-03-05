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

package com.rapidclipse.framework.server.mobilekit.vibration;

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * This service provides a way to vibrate the device.
 *
 * @author XDEV Software
 *
 */
@ServiceComponent(VibrationComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-vibration", spec = "3.1.0"))
public interface VibrationService extends MobileService
{
	public static VibrationService getCurrent()
	{
		return MobileService.getCurrent(VibrationService.class);
	}
	
	/**
	 * Vibrates with the specified pattern.
	 * <p>
	 * Single vibration for one second:
	 *
	 * <pre>
	 * {@code
	 * vibrate(1000);
	 * }
	 * </pre>
	 * <p>
	 * Vibrate for one second,<br>
	 * Pause for half a second,<br>
	 * Vibrate for 200 milliseconds:
	 *
	 * <pre>
	 * {@code
	 * vibrate(1000,500,200);
	 * }
	 * </pre>
	 *
	 *
	 * @param pattern
	 *            time/pause pattern to vibrate in milliseconds
	 */
	public void vibrate(int... pattern);
}

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

/**
 * Information about the devices hardware and software.
 *
 *
 * @author XDEV Software
 *
 */
public interface DeviceInfo
{
	/**
	 * Returns the name of the device's model or product. The value is set by
	 * the device manufacturer and may be different across versions of the same
	 * product.
	 */
	public String getModel();
	
	/**
	 * Returns the device's operating system name.
	 */
	public String getPlatform();
	
	/**
	 * Returns the device's Universally Unique Identifier (UUID).
	 */
	public String getUuid();
	
	/**
	 * Returns the operating system version.
	 */
	public String getVersion();
	
	/**
	 * Returns the device's manufacturer.
	 */
	public String getManufacturer();
	
	/**
	 * Returns whether the device is running on a simulator.
	 */
	public boolean isVirtual();
	
	/**
	 * Returns the device hardware serial number.
	 */
	public String getSerial();
}

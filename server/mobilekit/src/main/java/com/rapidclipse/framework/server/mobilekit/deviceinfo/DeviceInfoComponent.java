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

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;

import elemental.json.JsonObject;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-deviceinfo")
@HtmlImport("deviceinfo.html")
public class DeviceInfoComponent extends MobileComponent implements DeviceInfoService
{
	public DeviceInfoComponent()
	{
		super();
	}

	@Override
	public void getDeviceInfo(final Consumer<DeviceInfo> callback)
	{
		final String id = registerCall(callback, null);
		getElement().callFunction("get", id);
	}

	@ClientCallable
	void callback(final String id, final JsonObject infoObj)
	{
		final DeviceInfo info = toJava(infoObj, DeviceInfoImpl.class);
		getAndRemoveCall(id).success(info);
	}

	private static class DeviceInfoImpl implements DeviceInfo
	{
		private final String  model;
		private final String  platform;
		private final String  uuid;
		private final String  version;
		private final String  manufacturer;
		private final boolean virtual;
		private final String  serial;

		@SuppressWarnings("unused") // Used by Gson via reflection
		DeviceInfoImpl(
			final String model,
			final String platform,
			final String uuid,
			final String version,
			final String manufacturer,
			final boolean virtual,
			final String serial)
		{
			this.model        = model;
			this.platform     = platform;
			this.uuid         = uuid;
			this.version      = version;
			this.manufacturer = manufacturer;
			this.virtual      = virtual;
			this.serial       = serial;
		}

		@Override
		public String getModel()
		{
			return this.model;
		}

		@Override
		public String getPlatform()
		{
			return this.platform;
		}

		@Override
		public String getUuid()
		{
			return this.uuid;
		}

		@Override
		public String getVersion()
		{
			return this.version;
		}

		@Override
		public String getManufacturer()
		{
			return this.manufacturer;
		}

		@Override
		public boolean isVirtual()
		{
			return this.virtual;
		}

		@Override
		public String getSerial()
		{
			return this.serial;
		}
	}
}

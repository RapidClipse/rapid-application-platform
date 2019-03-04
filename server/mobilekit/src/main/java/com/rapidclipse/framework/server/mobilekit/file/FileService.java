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

package com.rapidclipse.framework.server.mobilekit.file;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * This service implements a File API allowing read access to files
 * residing on the device.
 *
 * @author XDEV Software
 *
 */
@ServiceComponent(FileComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-file", spec = "6.0.1"))
public interface FileService extends MobileService
{
	public static FileService getCurrent()
	{
		return MobileService.getCurrent(FileService.class);
	}
	
	default public void readFile(final String path, final Consumer<FileData> successCallback)
	{
		readFile(path, successCallback, null);
	}
	
	public void readFile(
		String path,
		Consumer<FileData> successCallback,
		Consumer<FileServiceError> errorCallback);
}

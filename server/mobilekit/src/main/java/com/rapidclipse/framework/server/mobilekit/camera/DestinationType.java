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

/**
 * The format of the return value the {@link CameraService} uses.
 *
 * @author XDEV Software
 *
 */
public enum DestinationType
{
	/**
	 * Return base64 encoded string
	 */
	IMAGE("DATA_URL"),
	
	/**
	 * Return file uri (content://media/external/images/media/2 for Android)
	 */
	FILE_URI("FILE_URI"),
	
	/**
	 * Return native uri (eg. asset-library://... for iOS)
	 */
	NATIVE_URI("NATIVE_URI");
	
	private String fieldName;

	private DestinationType(final String fieldName)
	{
		this.fieldName = fieldName;
	}

	String getFieldName()
	{
		return this.fieldName;
	}
}

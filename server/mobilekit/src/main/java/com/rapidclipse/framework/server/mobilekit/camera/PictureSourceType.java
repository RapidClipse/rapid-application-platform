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
 * Set the source of the picture.
 *
 * @author XDEV Software
 *
 */
public enum PictureSourceType
{
	/**
	 * Choose image from picture library (same as
	 * {@link PictureSourceType#SAVED_PHOTO_ALBUM} for Android)
	 */
	PHOTO_LIBRARY("PHOTOLIBRARY"),
	
	/**
	 * Take picture from camera
	 */
	CAMERA("CAMERA"),
	
	/**
	 * Choose image from picture library (same as
	 * {@link PictureSourceType#PHOTO_LIBRARY} for Android)
	 */
	SAVED_PHOTO_ALBUM("SAVEDPHOTOALBUM");
	
	private String fieldName;

	private PictureSourceType(final String fieldName)
	{
		this.fieldName = fieldName;
	}

	public String getFieldName()
	{
		return this.fieldName;
	}
}

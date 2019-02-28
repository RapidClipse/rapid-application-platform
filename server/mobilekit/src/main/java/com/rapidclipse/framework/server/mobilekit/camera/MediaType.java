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
 * Set the type of media to select from.
 *
 * @author XDEV Software
 *
 */
public enum MediaType
{
	/**
	 * Allow selection of still pictures only. DEFAULT. Will return format
	 * specified via {@link DestinationType}.
	 */
	PICTURE,
	
	/**
	 * Allow selection of video only, ONLY RETURNS URL.
	 */
	VIDEO,
	
	/**
	 * Allow selection from all media types.
	 */
	ALLMEDIA
}

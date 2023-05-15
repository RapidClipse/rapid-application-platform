/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.webapi.video;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.List;


/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class VideoWrapper implements Serializable
{
	private final String       mimeType;
	private final List<byte[]> data;
	
	public VideoWrapper(final String mimeType, final List<byte[]> data)
	{
		this.mimeType = mimeType;
		this.data     = data;
	}
	
	public String getMimeType()
	{
		return this.mimeType;
	}
	
	public List<byte[]> getData()
	{
		return this.data;
	}
	
	public void saveToFile(final File target, final boolean append) throws FileNotFoundException, IOException
	{
		try(final OutputStream out = new FileOutputStream(target, append))
		{
			// Ranged for-loop as we would have to catch the exception otherwise
			for(final byte[] b : this.data)
			{
				out.write(b);
			}
		}
	}
}

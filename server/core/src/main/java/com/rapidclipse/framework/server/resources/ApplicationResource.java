/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.resources;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;

import javax.servlet.ServletContext;

import com.vaadin.flow.server.StreamResource;
import com.vaadin.flow.server.VaadinServlet;


/**
 * @author XDEV Software
 *
 */
public class ApplicationResource extends StreamResource
{
	public ApplicationResource(final Class<?> requestor, final String path)
	{
		super(getFileName(path), () -> createInputStream(requestor, path));
	}

	private static String getFileName(final String path)
	{
		int i = path.lastIndexOf('/');
		if(i == -1)
		{
			i = path.lastIndexOf('\\');
		}
		if(i != -1)
		{
			return path.substring(i + 1);
		}
		return path;
	}

	public static InputStream createInputStream(final Class<?> requestor, final String path)
	{
		InputStream stream = getInputStream(requestor, path);
		if(stream != null)
		{
			return stream;
		}

		final String webContent = "WebContent/";
		if(path.startsWith(webContent))
		{
			stream = getInputStream(requestor, path.substring(webContent.length()));
			if(stream != null)
			{
				return stream;
			}
		}

		throw new RuntimeException("'" + path + "' could not be found in application.");
	}

	private static InputStream getInputStream(final Class<?> requestor, final String path)
	{
		try
		{
			final File file = new File(getRootPath(VaadinServlet.getCurrent().getServletContext()),
				path);
			if(file.exists())
			{
				return new FileInputStream(file);
			}
		}
		catch(final IOException e)
		{
		}

		Class<?> clazz = requestor;
		if(clazz == null)
		{
			clazz = ApplicationResource.class;
		}
		InputStream stream = clazz.getResourceAsStream(path);
		if(stream == null)
		{
			if(path.startsWith("/"))
			{
				stream = clazz.getResourceAsStream(path.substring(1));
			}
			else
			{
				stream = clazz.getResourceAsStream("/" + path);
			}
		}

		return stream;
	}

	private static String getRootPath(final ServletContext servletContext)
		throws MalformedURLException
	{
		final String rootPath = servletContext.getRealPath("/");
		if(rootPath != null)
		{
			return rootPath;
		}

		return servletContext.getResource("/").getFile();
	}
}

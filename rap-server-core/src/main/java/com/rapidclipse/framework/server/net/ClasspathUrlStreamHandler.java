/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.net;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;


/**
 * @author XDEV Software
 */
public class ClasspathUrlStreamHandler extends URLStreamHandler
{
	@Override
	protected URLConnection openConnection(final URL url) throws IOException
	{
		final String path = url.getPath();
		
		URL classpathUrl = Thread.currentThread().getContextClassLoader().getResource(path);
		if(classpathUrl == null)
		{
			classpathUrl = ClasspathUrlStreamHandler.class.getResource(path);
		}
		
		if(classpathUrl == null)
		{
			throw new FileNotFoundException(path);
		}
		
		return classpathUrl.openConnection();
	}
}

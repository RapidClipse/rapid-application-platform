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
		final String path         = url.getPath();
		
		URL          classpathUrl = Thread.currentThread().getContextClassLoader().getResource(path);
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

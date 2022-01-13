/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.net;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;

import javax.servlet.ServletException;

import com.rapidclipse.framework.server.RapServlet;


/**
 * @author XDEV Software
 */
public class ClasspathUrlStreamHandlerFactory implements URLStreamHandlerFactory
{
	public static class Initializer implements RapServlet.Extension
	{
		@Override
		public void servletInitialized(final RapServlet servlet) throws ServletException
		{
			installIfNeeded();
		}
	}
	
	private static boolean installed = false;
	
	public static void installIfNeeded()
	{
		if(installed)
		{
			return;
		}
		
		try
		{
			new URL("classpath:test").openConnection();
		}
		catch(final MalformedURLException e)
		{
			// no classpath handler available
			install();
		}
		catch(final IOException e)
		{
		}
	}
	
	public static void install()
	{
		if(installed)
		{
			return;
		}
		
		try
		{
			final Field field = URL.class.getDeclaredField("factory");
			field.setAccessible(true);
			final URLStreamHandlerFactory factory = (URLStreamHandlerFactory)field.get(null);
			field.set(null, new ClasspathUrlStreamHandlerFactory(factory));
			
			installed = true;
		}
		catch(NoSuchFieldException | SecurityException | IllegalArgumentException
			| IllegalAccessException e)
		{
			throw new RuntimeException(e);
		}
	}
	
	private final URLStreamHandlerFactory   delegate;
	private final ClasspathUrlStreamHandler classpathURLStreamHandler = new ClasspathUrlStreamHandler();
	
	public ClasspathUrlStreamHandlerFactory(final URLStreamHandlerFactory delegate)
	{
		super();
		this.delegate = delegate;
	}
	
	@Override
	public URLStreamHandler createURLStreamHandler(final String protocol)
	{
		if(protocol.toLowerCase().equals("classpath"))
		{
			return this.classpathURLStreamHandler;
		}
		
		if(this.delegate != null)
		{
			return this.delegate.createURLStreamHandler(protocol);
		}
		
		return null;
	}
}

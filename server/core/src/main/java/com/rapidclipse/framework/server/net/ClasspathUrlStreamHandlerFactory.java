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

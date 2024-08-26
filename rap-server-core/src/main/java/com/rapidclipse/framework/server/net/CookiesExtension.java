/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.net;

import jakarta.servlet.ServletException;

import com.rapidclipse.framework.server.RapServlet;


/**
 * @author XDEV Software
 *
 */
public class CookiesExtension implements RapServlet.Extension
{
	/**
	 *
	 */
	public CookiesExtension()
	{
		super();
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void servletInitialized(final RapServlet servlet) throws ServletException
	{
		servlet.getService().addSessionInitListener(event -> Cookies.initFor(event.getSession()));
	}
}

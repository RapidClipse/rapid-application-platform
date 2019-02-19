/*-
 * ---
 * Rapid Application Platform / Server / Core
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.net;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.rapidclipse.framework.server.RapServlet;


/**
 * @author XDEV Software
 *
 */
public class CorsExtension implements RapServlet.Extension
{
	/**
	 *
	 */
	public CorsExtension()
	{
		super();
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean handleHttpRequest(
		final HttpServletRequest request,
		final HttpServletResponse response)
		throws ServletException, IOException
	{
		final String origin = request.getHeader("Origin");
		if(origin != null)
		{
			// Handle a preflight "option" requests
			final String requestMethod = request.getMethod();
			if("options".equalsIgnoreCase(requestMethod))
			{
				response.addHeader("Access-Control-Allow-Origin", origin);
				response.setHeader("Allow", "GET, HEAD, POST, PUT, DELETE,TRACE,OPTIONS");
				
				// allow the requested method
				final String method = request.getHeader("Access-Control-Request-Method");
				response.addHeader("Access-Control-Allow-Methods", method);
				
				// allow the requested headers
				final String headers = request.getHeader("Access-Control-Request-Headers");
				response.addHeader("Access-Control-Allow-Headers", headers);
				
				response.addHeader("Access-Control-Allow-Credentials", "true");
				response.setContentType("text/plain");
				response.setCharacterEncoding("utf-8");
				response.getWriter().flush();
				
				// end of response
				return true;
			}
			// Handle UIDL post requests
			else if("post".equalsIgnoreCase(requestMethod))
			{
				response.addHeader("Access-Control-Allow-Origin", origin);
				response.addHeader("Access-Control-Allow-Credentials", "true");
			}
		}
		
		return false;
	}
}

/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
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
	public boolean handleHttpRequest(final HttpServletRequest request,
			final HttpServletResponse response) throws ServletException, IOException
	{
		final String origin = request.getHeader("Origin");
		if(origin != null)
		{
			// Handle a preflight "option" requests
			final String requestMethod = request.getMethod();
			if("options".equalsIgnoreCase(requestMethod))
			{
				response.addHeader("Access-Control-Allow-Origin",origin);
				response.setHeader("Allow","GET, HEAD, POST, PUT, DELETE,TRACE,OPTIONS");

				// allow the requested method
				final String method = request.getHeader("Access-Control-Request-Method");
				response.addHeader("Access-Control-Allow-Methods",method);

				// allow the requested headers
				final String headers = request.getHeader("Access-Control-Request-Headers");
				response.addHeader("Access-Control-Allow-Headers",headers);

				response.addHeader("Access-Control-Allow-Credentials","true");
				response.setContentType("text/plain");
				response.setCharacterEncoding("utf-8");
				response.getWriter().flush();

				// end of response
				return true;
			}
			// Handle UIDL post requests
			else if("post".equalsIgnoreCase(requestMethod))
			{
				response.addHeader("Access-Control-Allow-Origin",origin);
				response.addHeader("Access-Control-Allow-Credentials","true");
			}
		}

		return false;
	}
}

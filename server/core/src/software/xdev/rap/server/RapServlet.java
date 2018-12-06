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

package software.xdev.rap.server;


import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.vaadin.flow.function.DeploymentConfiguration;
import com.vaadin.flow.server.ServiceException;
import com.vaadin.flow.server.VaadinServlet;

import software.xdev.rap.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public class RapServlet extends VaadinServlet
{
	public static interface Extension
	{
		public default void servletInitialized(final RapServlet servlet) throws ServletException
		{
		}


		public default boolean handleHttpRequest(final HttpServletRequest request,
				final HttpServletResponse response) throws ServletException, IOException
		{
			return false;
		}
	}


	///////////////////////////////////////////////////////////////////////////
	// static methods//
	/////////////////////////////////////////////////

	public static RapServlet getRapServlet()
	{
		return (RapServlet)VaadinServlet.getCurrent();
	}
	
	
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////////////////////////////////////

	/**
	 *
	 */
	public RapServlet()
	{
		super();
	}

	///////////////////////////////////////////////////////////////////////////
	// overrides //
	/////////////////////////////////////////////////


	/**
	 * {@inheritDoc}
	 */
	@Override
	protected RapServletService createServletService(
			final DeploymentConfiguration deploymentConfiguration) throws ServiceException
	{
		final RapServletService service = new RapServletService(this,deploymentConfiguration);
		service.init();
		return service;
	}


	/**
	 * {@inheritDoc}
	 */
	@Override
	public RapServletService getService()
	{
		return (RapServletService)super.getService();
	}


	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void servletInitialized() throws ServletException
	{
		super.servletInitialized();

		for(final Extension extension : ServiceLoader.forType(Extension.class).services())
		{
			extension.servletInitialized(this);
		}
	}


	/**
	 * {@inheritDoc}
	 */
	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response)
			throws ServletException, IOException
	{
		for(final Extension extension : ServiceLoader.forType(Extension.class).services())
		{
			if(extension.handleHttpRequest(request,response))
			{
				return;
			}
		}

		super.service(request,response);
	}
}

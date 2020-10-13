/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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

package com.rapidclipse.framework.server;

import java.io.IOException;
import java.util.Properties;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.resources.StringResourceI18NProvider;
import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.function.DeploymentConfiguration;
import com.vaadin.flow.server.InitParameters;
import com.vaadin.flow.server.ServiceException;
import com.vaadin.flow.server.VaadinServlet;


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
		
		public default boolean handleHttpRequest(
			final HttpServletRequest request,
			final HttpServletResponse response)
			throws ServletException, IOException
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
	protected DeploymentConfiguration createDeploymentConfiguration(final Properties initParameters)
	{
		final DeploymentConfiguration deploymentConfiguration = super.createDeploymentConfiguration(initParameters);
		
		/*
		 * Inject default I18Provider if none has been set
		 */
		if(StringUtils.isEmpty(deploymentConfiguration.getStringProperty(InitParameters.I18N_PROVIDER, null)))
		{
			System.setProperty("vaadin." + InitParameters.I18N_PROVIDER, StringResourceI18NProvider.class.getName());
		}
		
		return deploymentConfiguration;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	protected RapServletService createServletService(
		final DeploymentConfiguration deploymentConfiguration)
		throws ServiceException
	{
		final RapServletService service = new RapServletService(this, deploymentConfiguration);
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
			if(extension.handleHttpRequest(request, response))
			{
				return;
			}
		}
		
		super.service(request, response);
	}
}

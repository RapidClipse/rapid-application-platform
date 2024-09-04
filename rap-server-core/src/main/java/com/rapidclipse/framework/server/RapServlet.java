/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server;

import java.io.IOException;
import java.util.Properties;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

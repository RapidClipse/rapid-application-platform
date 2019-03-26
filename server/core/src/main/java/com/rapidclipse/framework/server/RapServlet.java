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
import com.vaadin.flow.server.Constants;
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
		if(StringUtils.isEmpty(deploymentConfiguration.getStringProperty(Constants.I18N_PROVIDER, null)))
		{
			System.setProperty("vaadin." + Constants.I18N_PROVIDER, StringResourceI18NProvider.class.getName());
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

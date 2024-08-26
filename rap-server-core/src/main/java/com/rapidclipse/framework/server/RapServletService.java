/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server;

import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.function.DeploymentConfiguration;
import com.vaadin.flow.router.RouteConfiguration;
import com.vaadin.flow.server.HandlerHelper;
import com.vaadin.flow.server.HandlerHelper.RequestType;
import com.vaadin.flow.server.ServiceException;
import com.vaadin.flow.server.SessionExpiredException;
import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinResponse;
import com.vaadin.flow.server.VaadinService;
import com.vaadin.flow.server.VaadinServletService;
import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 *
 */
public class RapServletService extends VaadinServletService
{
	public static interface Extension
	{
		public default void serviceInitialized(final RapServletService service)
		{
		}
		
		public default void sessionCreated(
			final RapServletService service,
			final VaadinSession session,
			final VaadinRequest request)
		{
		}
		
		public default void onRequestStart(
			final RapServletService service,
			final VaadinSession session)
		{
		}
		
		public default void onRequestEnd(
			final RapServletService service,
			final VaadinSession session)
		{
		}
	}
	
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////////////////////////////////////
	
	/**
	 * @param servlet
	 * @param deploymentConfiguration
	 */
	public RapServletService(
		final RapServlet servlet,
		final DeploymentConfiguration deploymentConfiguration)
	{
		super(servlet, deploymentConfiguration);
	}
	
	///////////////////////////////////////////////////////////////////////////
	// overrides //
	/////////////////////////////////////////////////
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public RapServlet getServlet()
	{
		return (RapServlet)super.getServlet();
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void init() throws ServiceException
	{
		super.init();
		
		ServiceLoader.forType(Extension.class).services()
			.forEach(extension -> extension.serviceInitialized(this));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	protected VaadinSession createVaadinSession(final VaadinRequest request)
	{
		final VaadinSession session = super.createVaadinSession(request);
		
		ServiceLoader.forType(Extension.class).services()
			.forEach(extension -> extension.sessionCreated(this, session, request));
		
		return session;
	}
	
	@Override
	public VaadinSession findVaadinSession(final VaadinRequest request) throws SessionExpiredException
	{
		final VaadinSession session = super.findVaadinSession(request);
		
		if(session != null && VaadinService.getCurrent() != null)
		{
			// auto-map * to <root>
			final RouteConfiguration routeConfiguration = RouteConfiguration.forApplicationScope();
			if(!routeConfiguration.getRoute("*").isPresent())
			{
				routeConfiguration.getRoute("").ifPresent(clazz -> routeConfiguration.setRoute("*", clazz));
			}
		}
		
		return session;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void requestStart(final VaadinRequest request, final VaadinResponse response)
	{
		super.requestStart(request, response);
		
		if(!HandlerHelper.isRequestType(request, RequestType.HEARTBEAT))
		{
			try
			{
				final VaadinSession session = findVaadinSession(request);
				if(session != null)
				{
					try
					{
						session.lock();
						
						ServiceLoader.forType(Extension.class).services()
							.forEach(extension -> extension.onRequestStart(this, session));
					}
					finally
					{
						session.unlock();
					}
				}
			}
			catch(final SessionExpiredException e)
			{
				throw new RuntimeException(e);
			}
		}
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void requestEnd(
		final VaadinRequest request,
		final VaadinResponse response,
		final VaadinSession session)
	{
		if(session != null && !HandlerHelper.isRequestType(request, RequestType.HEARTBEAT))
		{
			try
			{
				session.lock();
				
				ServiceLoader.forType(Extension.class).services()
					.forEach(extension -> extension.onRequestEnd(this, session));
			}
			finally
			{
				session.unlock();
			}
		}
		
		super.requestEnd(request, response, session);
	}
}

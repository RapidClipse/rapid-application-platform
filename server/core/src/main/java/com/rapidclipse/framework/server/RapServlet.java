
package com.rapidclipse.framework.server;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.function.DeploymentConfiguration;
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

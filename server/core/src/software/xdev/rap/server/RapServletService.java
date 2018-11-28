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


import com.vaadin.flow.function.DeploymentConfiguration;
import com.vaadin.flow.server.ServiceException;
import com.vaadin.flow.server.ServletHelper;
import com.vaadin.flow.server.ServletHelper.RequestType;
import com.vaadin.flow.server.SessionExpiredException;
import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinResponse;
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
		
		
		public default void sessionCreated(final RapServletService service,
				final VaadinSession session, final VaadinRequest request)
		{
		}
		
		
		public default void onRequestStart(final RapServletService service,
				final VaadinSession session)
		{
		}
		
		
		public default void onRequestEnd(final RapServletService service,
				final VaadinSession session)
		{
		}
	}
	
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	/////////////////////////////////////////////////
	
	private final ServiceLoader<Extension> serviceLoader = ServiceLoader.For(Extension.class);
	
	
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////////////////////////////////////
	
	/**
	 * @param servlet
	 * @param deploymentConfiguration
	 */
	public RapServletService(final RapServlet servlet,
			final DeploymentConfiguration deploymentConfiguration)
	{
		super(servlet,deploymentConfiguration);
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
		
		this.serviceLoader.services().forEach(extension -> extension.serviceInitialized(this));
	}
	
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	protected VaadinSession createVaadinSession(final VaadinRequest request)
	{
		final VaadinSession session = super.createVaadinSession(request);
		
		this.serviceLoader.services()
				.forEach(extension -> extension.sessionCreated(this,session,request));
		
		return session;
	}
	
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void requestStart(final VaadinRequest request, final VaadinResponse response)
	{
		super.requestStart(request,response);
		
		if("POST".equalsIgnoreCase(request.getMethod())
				&& !ServletHelper.isRequestType(request,RequestType.HEARTBEAT))
		{
			try
			{
				final VaadinSession session = findVaadinSession(request);
				if(session != null)
				{
					this.serviceLoader.services()
							.forEach(extension -> extension.onRequestStart(this,session));
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
	public void requestEnd(final VaadinRequest request, final VaadinResponse response,
			final VaadinSession session)
	{
		
		if(session != null && !ServletHelper.isRequestType(request,RequestType.HEARTBEAT))
		{
			try
			{
				session.lock();

				this.serviceLoader.services()
						.forEach(extension -> extension.onRequestEnd(this,session));
			}
			finally
			{
				session.unlock();
			}
		}
		
		super.requestEnd(request,response,session);
	}
}

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


import java.util.function.Supplier;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import org.apache.commons.lang3.StringUtils;

import com.vaadin.flow.server.VaadinSession;

import software.xdev.rap.server.concurrent.RapExecutorService;
import software.xdev.rap.server.net.ContentSecurityPolicy;


/**
 * @author XDEV Software
 *
 */
public final class Rap
{
	private static RapExecutorService		executorService;
	private static ContentSecurityPolicy	contentSecurityPolicy;


	/**
	 * @return the executorService
	 */
	public static RapExecutorService getExecutorService()
	{
		if(executorService == null)
		{
			executorService = createXdevExecutorService(
					RapServlet.getRapServlet().getServletContext());
		}
		return executorService;
	}


	private static RapExecutorService createXdevExecutorService(final ServletContext context)
	{
		final String className = context
				.getInitParameter(RapExecutorService.FACTORY_INIT_PARAMETER);
		if(!StringUtils.isEmpty(className))
		{
			try
			{
				final RapExecutorService.Factory factory = (RapExecutorService.Factory)Class
						.forName(className).newInstance();
				return factory.createExecutorService(context);
			}
			catch(final Throwable t)
			{
				throw new RuntimeException(t);
			}
		}

		return new RapExecutorService.Implementation(context);
	}


	/**
	 * @return the contentSecurityPolicy
	 */
	public static ContentSecurityPolicy getContentSecurityPolicy()
	{
		return contentSecurityPolicy;
	}


	/**
	 * @param contentSecurityPolicy
	 *            the contentSecurityPolicy to set
	 */
	public static void setContentSecurityPolicy(final ContentSecurityPolicy contentSecurityPolicy)
	{
		Rap.contentSecurityPolicy = contentSecurityPolicy;
	}


	public static <T> T sessionBoundInstance(final Class<T> type, final Supplier<T> instantiator)
	{
		final VaadinSession session = VaadinSession.getCurrent();
		if(session == null)
		{
			return null;
		}
		T instance = session.getAttribute(type);
		if(instance == null)
		{
			instance = instantiator.get();
			session.setAttribute(type,instance);
		}
		return instance;
	}
	
	
	
	@WebListener
	public static class ContextListener implements ServletContextListener
	{
		@Override
		public void contextInitialized(final ServletContextEvent sce)
		{
		}
		
		
		@Override
		public void contextDestroyed(final ServletContextEvent sce)
		{
			if(Rap.executorService != null)
			{
				Rap.executorService.shutdown();
				Rap.executorService = null;
			}
		}
	}


	private Rap()
	{
		throw new Error();
	}
}

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


import javax.servlet.ServletContext;

import org.apache.commons.lang3.StringUtils;

import software.xdev.rap.server.concurrent.RapExecutorService;


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


	private Rap()
	{
		throw new Error();
	}
}

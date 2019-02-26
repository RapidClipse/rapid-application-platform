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

import static com.rapidclipse.framework.server.util.StacktraceUtils.cutStacktraceByOne;

import java.util.Collection;
import java.util.function.Supplier;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.concurrent.RapExecutorService;
import com.rapidclipse.framework.server.net.ContentSecurityPolicy;
import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 *
 */
public final class Rap
{
	private static RapExecutorService    executorService;
	private static ContentSecurityPolicy contentSecurityPolicy;

	/**
	 * @return the executorService
	 */
	public static RapExecutorService getExecutorService()
	{
		if(executorService == null)
		{
			executorService = createExecutorService(
				RapServlet.getRapServlet().getServletContext());
		}
		return executorService;
	}

	private static RapExecutorService createExecutorService(final ServletContext context)
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
			session.setAttribute(type, instance);
		}
		return instance;
	}

	public static <E> E[] notEmpty(final E[] array)
	{
		if(array.length == 0)
		{
			throw cutStacktraceByOne(new IllegalArgumentException());
		}
		return array;
	}

	public static <E, C extends Collection<E>> C notEmpty(final C collection)
	{
		if(collection.isEmpty())
		{
			throw cutStacktraceByOne(new IllegalArgumentException());
		}
		return collection;
	}

	public static <E> E[] minLength(final E[] array, final int minLength)
	{
		if(array.length < minLength)
		{
			throw cutStacktraceByOne(new IllegalArgumentException());
		}
		return array;
	}

	public static <E, C extends Collection<E>> C minSize(final C collection, final int minSize)
	{
		if(collection.size() < minSize)
		{
			throw cutStacktraceByOne(new IllegalArgumentException());
		}
		return collection;
	}

	public static Class<?> wrapperTypeIfPrimitive(final Class<?> clazz)
	{
		if(clazz.isPrimitive())
		{
			return wrapperType(clazz);
		}

		return clazz;
	}

	public static Class<?> wrapperType(final Class<?> primitive)
	{
		if(primitive == int.class)
		{
			return Integer.class;
		}
		if(primitive == double.class)
		{
			return Double.class;
		}
		if(primitive == boolean.class)
		{
			return Boolean.class;
		}
		if(primitive == float.class)
		{
			return Float.class;
		}
		if(primitive == byte.class)
		{
			return Byte.class;
		}
		if(primitive == long.class)
		{
			return Long.class;
		}
		if(primitive == short.class)
		{
			return Short.class;
		}
		if(primitive == char.class)
		{
			return Character.class;
		}

		throw new IllegalArgumentException("Not a primitive: " + primitive.getName());
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

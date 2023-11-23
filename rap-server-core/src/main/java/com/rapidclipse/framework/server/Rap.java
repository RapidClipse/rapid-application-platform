/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server;

import static com.rapidclipse.framework.server.util.StacktraceUtils.cutStacktraceByOne;

import java.util.Collection;
import java.util.function.Function;
import java.util.function.Supplier;

import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletContextEvent;
import jakarta.servlet.ServletContextListener;
import jakarta.servlet.annotation.WebListener;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.concurrent.RapExecutorService;
import com.rapidclipse.framework.server.data.DAO;
import com.rapidclipse.framework.server.data.DataAccessObject;
import com.rapidclipse.framework.server.net.ContentSecurityPolicy;
import com.rapidclipse.framework.server.util.SoftCache;
import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 *
 */
public final class Rap
{
	private static RapExecutorService                             executorService;
	private static ContentSecurityPolicy                          contentSecurityPolicy;
	private final static SoftCache<Class<?>, DataAccessObject<?>> daoCache = new SoftCache<>();

	/**
	 * @return the executorService
	 */
	public static RapExecutorService getExecutorService()
	{
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

		return RapExecutorService.New(context);
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

	public static <T> T ensureSessionInstance(final Class<T> type, final Function<VaadinSession, T> creator)
	{
		final VaadinSession session  = VaadinSession.getCurrent();
		T                   instance = session.getAttribute(type);
		if(instance == null)
		{
			instance = creator.apply(session);
			session.setAttribute(type, instance);
		}
		return instance;
	}

	public static <D extends DataAccessObject<?>> D getDao(final Class<D> daoType)
		throws RuntimeException
	{
		synchronized(daoCache)
		{
			@SuppressWarnings("unchecked")
			D dao = (D)daoCache.get(daoType);

			if(dao == null)
			{
				try
				{
					dao = daoType.newInstance();
					daoCache.put(daoType, dao);
				}
				catch(InstantiationException | IllegalAccessException e)
				{
					throw new RuntimeException(e);
				}
			}

			return dao;
		}
	}

	@SuppressWarnings("unchecked")
	public static <T> DataAccessObject<T> getDao(final T pojo)
		throws RuntimeException
	{
		return (DataAccessObject<T>)getDaoByPojoType(pojo.getClass());
	}

	@SuppressWarnings("unchecked")
	public static <T> DataAccessObject<T> getDaoByPojoType(final Class<T> pojo)
		throws RuntimeException
	{
		final DAO dao = pojo.getAnnotation(DAO.class);
		if(dao == null)
		{
			throw new IllegalArgumentException("No DAO annotation present");
		}

		return (DataAccessObject<T>)getDao(dao.value());
	}

	@WebListener
	public static class ContextListener implements ServletContextListener
	{
		@Override
		public void contextInitialized(final ServletContextEvent event)
		{
			if(Rap.executorService == null)
			{
				Rap.executorService = Rap.createExecutorService(event.getServletContext());
			}
		}

		@Override
		public void contextDestroyed(final ServletContextEvent event)
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

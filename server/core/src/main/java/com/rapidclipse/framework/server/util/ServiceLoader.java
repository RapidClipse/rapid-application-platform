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

package com.rapidclipse.framework.server.util;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import com.rapidclipse.framework.server.util.SoftCache.Equality;


/**
 *
 * @author XDEV Software
 *
 * @see ServicePriority
 *
 */
public interface ServiceLoader<T>
{
	public Iterable<T> services();

	public Stream<T> servicesStream();

	public Iterable<T> servicesUncached();

	public Stream<T> servicesStreamUncached();

	public static <T> ServiceLoader<T> forType(final Class<T> type)
	{
		return CACHE.getOrCreate(type);
	}

	///////////////////////////////////////////////////////////////////////////
	// implementation //
	/////////////////////////////////////////////////

	public static class Default<T> implements ServiceLoader<T>
	{
		private final Class<T> type;
		private List<T>        services;

		protected Default(final Class<T> type)
		{
			this.type = type;
		}

		@Override
		public Iterable<T> services()
		{
			return getServices();
		}

		@Override
		public Stream<T> servicesStream()
		{
			return getServices().stream();
		}

		private List<T> getServices()
		{
			if(this.services == null)
			{
				this.services = readServices();
			}

			return this.services;
		}

		@Override
		public Iterable<T> servicesUncached()
		{
			return readServices();
		}

		@Override
		public Stream<T> servicesStreamUncached()
		{
			return readServices().stream();
		}

		private List<T> readServices()
		{
			final List<T> list = new ArrayList<>();
			java.util.ServiceLoader.load(this.type, getClass().getClassLoader()).forEach(list::add);
			list.sort((s1, s2) -> Integer.compare(getPriority(s2), getPriority(s1)));
			return list;
		}

		private int getPriority(final T service)
		{
			final ServicePriority priority = service.getClass()
				.getAnnotation(ServicePriority.class);
			return priority != null ? priority.value() : ServicePriority.DEFAULT;
		}
	}

	///////////////////////////////////////////////////////////////////////////
	// Cache //
	/////////////////////////////////////////////////

	static Cache CACHE = new Cache();

	public final static class Cache
	{
		private final SoftCache<Class<?>, ServiceLoader<?>> cache = new SoftCache<>(
			Equality.IDENTITY);

		@SuppressWarnings("unchecked")
		synchronized <T> ServiceLoader<T> getOrCreate(final Class<T> type)
		{
			ServiceLoader<T> serviceLoader = (ServiceLoader<T>)this.cache.get(type);
			if(serviceLoader == null)
			{
				this.cache.put(type, serviceLoader = new Default<>(type));
			}
			return serviceLoader;
		}
	}
}

/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
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

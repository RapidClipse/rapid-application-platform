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


import java.util.ArrayList;
import java.util.List;


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
	
	
	public Iterable<T> servicesUncached();
	
	
	public static <T> ServiceLoader<T> For(final Class<T> type)
	{
		return new Implementation<>(type);
	}
	
	
	
	///////////////////////////////////////////////////////////////////////////
	// implementation //
	/////////////////////////////////////////////////
	
	public static class Implementation<T> implements ServiceLoader<T>
	{
		private final Class<T>	type;
		private List<T>			services;
		
		
		public Implementation(final Class<T> type)
		{
			this.type = type;
		}
		
		
		@Override
		public Iterable<T> services()
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
		
		
		private List<T> readServices()
		{
			final List<T> list = new ArrayList<>();
			java.util.ServiceLoader.load(this.type,getClass().getClassLoader()).forEach(list::add);
			list.sort((s1, s2) -> Integer.compare(getPriority(s2),getPriority(s1)));
			return list;
		}
		
		
		private int getPriority(final T service)
		{
			final ServicePriority priority = service.getClass()
					.getAnnotation(ServicePriority.class);
			return priority != null ? priority.value() : ServicePriority.DEFAULT;
		}
	}
}

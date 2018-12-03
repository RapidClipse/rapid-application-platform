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

package software.xdev.rap.server.navigation;


import static java.util.Objects.requireNonNull;
import static software.xdev.rap.server.Rap.sessionBoundInstance;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

import software.xdev.rap.server.data.ValueTransfer;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParameterRegistry extends Serializable
{
	public String put(final NavigationParameters parameters);


	public NavigationParameters get(final String id);


	public static NavigationParameterRegistry getCurrent()
	{
		return sessionBoundInstance(NavigationParameterRegistry.class,Implementation::new);
	}



	public static class Implementation implements NavigationParameterRegistry
	{
		private final Map<String, NavigationParameters> map = new HashMap<>();


		public Implementation()
		{
			super();
		}


		@Override
		public synchronized String put(final NavigationParameters parameters)
		{
			requireNonNull(parameters);

			final String id = getNewId();

			this.map.put(id,transform(parameters,ValueTransfer::put));

			return id;
		}


		@Override
		public synchronized NavigationParameters get(final String id)
		{
			final NavigationParameters parameters = this.map.get(id);
			if(parameters == null)
			{
				throw new NavigationException("Navigation state not found: " + id);
			}

			return transform(parameters,ValueTransfer::get);
		}
		
		
		protected String getNewId()
		{
			String id;
			do
			{
				id = UUID.randomUUID().toString();
			}
			while(this.map.containsKey(id));
			
			return id;
		}


		protected NavigationParameters transform(final NavigationParameters parameters,
				final Function<Object, Object> logic)
		{
			final Map<String, Object> transformed = new HashMap<>();
			
			for(final String name : parameters.names())
			{
				final Object value = parameters.value(name);
				final Object transformedValue = logic.apply(value);
				transformed.put(name,transformedValue);
			}
			
			return NavigationParameters.New(transformed);
		}
	}
}

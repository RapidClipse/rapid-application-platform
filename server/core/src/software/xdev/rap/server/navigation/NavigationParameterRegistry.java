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

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParameterRegistry
{
	public static NavigationParameterRegistry get(final VaadinSession session)
	{
		NavigationParameterRegistry registry = session
				.getAttribute(NavigationParameterRegistry.class);
		if(registry == null)
		{
			registry = new Implementation();
			session.setAttribute(NavigationParameterRegistry.class,registry);
		}
		return registry;
	}


	public String put(final NavigationParameters parameters);


	public NavigationParameters get(final String id);



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

			String id = null;
			while(id == null || this.map.containsKey(id))
			{
				id = UUID.randomUUID().toString();
			}

			this.map.put(id,parameters);
			
			return id;
		}


		@Override
		public synchronized NavigationParameters get(final String id)
		{
			return this.map.get(id);
		}
	}
}

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


import static java.util.Collections.unmodifiableMap;
import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.Map;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParameters extends Serializable
{
	public Iterable<String> names();


	public Object value(final String name);


	public static NavigationParameters New(final Map<String, Object> mapping)
	{
		return new Implementation(mapping);
	}
	
	
	
	public static class Implementation implements NavigationParameters
	{
		private final Map<String, Object> mapping;


		public Implementation(final Map<String, Object> mapping)
		{
			super();
			this.mapping = unmodifiableMap(requireNonNull(mapping));
		}


		@Override
		public Iterable<String> names()
		{
			return this.mapping.keySet();
		}


		@Override
		public Object value(final String name)
		{
			return this.mapping.get(name);
		}
	}
}

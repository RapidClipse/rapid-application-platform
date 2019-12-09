/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.navigation;

import static com.rapidclipse.framework.server.Rap.sessionBoundInstance;
import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

import com.rapidclipse.framework.server.data.ValueTransfer;


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
		return sessionBoundInstance(NavigationParameterRegistry.class, Default::new);
	}

	public static class Default implements NavigationParameterRegistry
	{
		private final Map<String, NavigationParameters> map = new HashMap<>();

		protected Default()
		{
			super();
		}

		@Override
		public synchronized String put(final NavigationParameters parameters)
		{
			requireNonNull(parameters);

			final String id = getNewId();

			this.map.put(id, transform(parameters, ValueTransfer::put));

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

			return transform(parameters, ValueTransfer::get);
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

		protected NavigationParameters transform(
			final NavigationParameters parameters,
			final Function<Object, Object> logic)
		{
			final Map<String, Object> transformed = new HashMap<>();

			for(final String name : parameters.names())
			{
				final Object value            = parameters.value(name);
				final Object transformedValue = logic.apply(value);
				transformed.put(name, transformedValue);
			}

			return NavigationParameters.New(transformed);
		}
	}
}

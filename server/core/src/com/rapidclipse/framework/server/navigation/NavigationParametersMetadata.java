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

package com.rapidclipse.framework.server.navigation;


import static java.util.Collections.unmodifiableMap;
import static java.util.Objects.requireNonNull;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParametersMetadata
{
	public Iterable<String> names();
	
	
	public NavigationParameterMetadata get(final String name);
	
	
	public List<String> mandatoryParameters();
	
	
	public static NavigationParametersMetadata New(final Class<?> targetType)
	{
		return New(NavigationUtils.getMetadata(targetType));
	}


	public static NavigationParametersMetadata New(
			final Map<String, NavigationParameterMetadata> parameters)
	{
		return new Implementation(parameters);
	}
	
	
	
	public static class Implementation implements NavigationParametersMetadata
	{
		private final Map<String, NavigationParameterMetadata> parameters;
		
		
		public Implementation(final Map<String, NavigationParameterMetadata> parameters)
		{
			super();
			this.parameters = unmodifiableMap(requireNonNull(parameters));
		}
		
		
		@Override
		public Iterable<String> names()
		{
			return this.parameters.keySet();
		}
		
		
		@Override
		public NavigationParameterMetadata get(final String name)
		{
			return this.parameters.get(name);
		}
		
		
		@Override
		public List<String> mandatoryParameters()
		{
			return this.parameters.entrySet().stream().filter(e -> !e.getValue().optional())
					.map(e -> e.getKey()).collect(Collectors.toList());
		}
	}
}

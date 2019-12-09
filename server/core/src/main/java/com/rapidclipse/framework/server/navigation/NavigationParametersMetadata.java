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

import static java.util.Collections.unmodifiableMap;
import static java.util.Objects.requireNonNull;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;


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
		return New(getMetadata(targetType));
	}
	
	public static Map<String, NavigationParameterMetadata> getMetadata(final Class<?> target)
	{
		final List<Class<?>> hierarchy = new ArrayList<>();
		Class<?>             clazz     = target;
		while(clazz != Object.class)
		{
			hierarchy.add(0, clazz);
			clazz = clazz.getSuperclass();
		}
		
		final Map<String, NavigationParameterMetadata> parameters = new HashMap<>();
		
		for(final Class<?> current : hierarchy)
		{
			for(final Field field : current.getDeclaredFields())
			{
				if(Modifier.isStatic(field.getModifiers()))
				{
					continue;
				}
				
				final NavigationParameter urlParameter = field
					.getAnnotation(NavigationParameter.class);
				if(urlParameter == null)
				{
					continue;
				}
				
				String name = urlParameter.name();
				if(StringUtils.isEmpty(name))
				{
					name = field.getName();
				}
				name = name.toLowerCase();
				parameters.put(name, NavigationParameterMetadata.New(field, field.getType(), name,
					urlParameter.optional()));
			}
			
			for(final Method method : current.getDeclaredMethods())
			{
				if(Modifier.isStatic(method.getModifiers())
					|| method.getParameterTypes().length != 1)
				{
					continue;
				}
				
				final NavigationParameter urlParameter = method
					.getAnnotation(NavigationParameter.class);
				if(urlParameter == null)
				{
					continue;
				}
				
				String name = urlParameter.name();
				if(StringUtils.isEmpty(name))
				{
					name = method.getName();
					if(name.startsWith("set") && name.length() > 3)
					{
						name = name.substring(3);
					}
				}
				name = name.toLowerCase();
				parameters.put(name, NavigationParameterMetadata.New(method,
					method.getParameterTypes()[0], name, urlParameter.optional()));
			}
		}
		
		return parameters;
	}

	public static NavigationParametersMetadata New(
		final Map<String, NavigationParameterMetadata> parameters)
	{
		return new Default(parameters);
	}

	public static class Default implements NavigationParametersMetadata
	{
		private final Map<String, NavigationParameterMetadata> parameters;

		protected Default(final Map<String, NavigationParameterMetadata> parameters)
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

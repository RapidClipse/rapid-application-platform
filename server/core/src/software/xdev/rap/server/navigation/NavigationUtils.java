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


import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import com.vaadin.flow.router.BeforeEvent;


/**
 * @author XDEV Software
 *
 */
final class NavigationUtils
{
	private NavigationUtils()
	{
	}

	public static String ID_PARAMETER_NAME = "_";


	public static Map<String, NavigationParameterMetadata> getMetadata(final Class<?> target)
	{
		final List<Class<?>> hierarchy = new ArrayList<>();
		Class<?> clazz = target;
		while(clazz != Object.class)
		{
			hierarchy.add(0,clazz);
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
				parameters.put(name,NavigationParameterMetadata.New(field,field.getType(),name,
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
				parameters.put(name,NavigationParameterMetadata.New(method,
						method.getParameterTypes()[0],name,urlParameter.optional()));
			}
		}

		return parameters;
	}


	public static void injectParameters(final Object target, final BeforeEvent event,
			final String parameter)
	{
		try
		{
			final NavigationParameterRegistry registry = NavigationParameterRegistry
					.get(event.getUI().getSession());
			final NavigationParameters mapping = registry.get(event.getLocation()
					.getQueryParameters().getParameters().get(ID_PARAMETER_NAME).get(0));
			final NavigationParametersMetadata metadata = NavigationParametersMetadata
					.New(target.getClass());
			for(final String name : mapping.names())
			{
				final Object value = mapping.value(name);
				final Member member = metadata.get(name).member();
				if(member instanceof Field)
				{
					final Field field = (Field)member;
					final boolean accessible = field.isAccessible();
					field.setAccessible(true);
					field.set(target,value);
					field.setAccessible(accessible);
				}
				else
				{
					final Method method = (Method)member;
					final boolean accessible = method.isAccessible();
					method.setAccessible(true);
					method.invoke(target,value);
					method.setAccessible(accessible);
				}
			}
		}
		catch(SecurityException | IllegalArgumentException | IllegalAccessException
				| InvocationTargetException e)
		{
			throw new NavigationException(e);
		}
	}
}

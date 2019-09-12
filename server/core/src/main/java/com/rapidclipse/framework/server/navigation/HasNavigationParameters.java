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
package com.rapidclipse.framework.server.navigation;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;


/**
 * @author XDEV Software
 *
 */
public interface HasNavigationParameters extends HasUrlParameter<String>
{
	@Override
	default void setParameter(final BeforeEvent event, final String parameter)
	{
		try
		{
			final NavigationParameterRegistry  registry = NavigationParameterRegistry.getCurrent();
			final NavigationParameters         mapping  = registry.get(event.getLocation()
				.getQueryParameters().getParameters().get(Navigation.ID_PARAMETER_NAME).get(0));
			final NavigationParametersMetadata metadata = NavigationParametersMetadata
				.New(this.getClass());
			for(final String name : mapping.names())
			{
				final Object value  = mapping.value(name);
				final Member member = metadata.get(name).member();
				if(member instanceof Field)
				{
					final Field   field      = (Field)member;
					final boolean accessible = field.isAccessible();
					try
					{
						field.setAccessible(true);
						field.set(this, value);
					}
					finally
					{
						field.setAccessible(accessible);
					}
				}
				else
				{
					final Method  method     = (Method)member;
					final boolean accessible = method.isAccessible();
					try
					{
						method.setAccessible(true);
						method.invoke(this, value);
					}
					finally
					{
						method.setAccessible(accessible);
					}
				}
			}
		}
		catch(SecurityException | IllegalArgumentException | IllegalAccessException
			| InvocationTargetException e)
		{
			throw new NavigationException(e);
		}
		
		navigationParametersUpdated();
	}
	
	default void navigationParametersUpdated()
	{
	}
}

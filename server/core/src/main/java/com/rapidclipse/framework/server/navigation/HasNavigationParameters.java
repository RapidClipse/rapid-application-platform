/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
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

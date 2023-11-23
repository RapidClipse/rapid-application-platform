/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.action;

import java.lang.reflect.Modifier;
import java.util.Set;

import jakarta.servlet.ServletContainerInitializer;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.HandlesTypes;

import com.rapidclipse.framework.server.util.ReflectionUtils;
import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.VaadinServiceInitListener;


/**
 * @author XDEV Software
 *
 */
@HandlesTypes(Action.class)
public class ActionRegistryInitializer implements ServletContainerInitializer, VaadinServiceInitListener
{
	public ActionRegistryInitializer()
	{
		super();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void onStartup(final Set<Class<?>> classSet, final ServletContext context) throws ServletException
	{
		if(classSet != null)
		{
			for(final Class<?> clazz : classSet)
			{
				if(!(clazz.isInterface() ||
					clazz.isEnum() ||
					clazz.isAnnotation() ||
					Modifier.isAbstract(clazz.getModifiers()) ||
					Action.class == clazz.getDeclaringClass() ||
					!ReflectionUtils.hasDefaultConstructor(clazz)))
				{
					ActionRegistry.registerActionType((Class<? extends Action>)clazz);
				}
			}
		}
	}
	
	@Override
	public void serviceInit(final ServiceInitEvent serviceEvent)
	{
		serviceEvent.getSource().addUIInitListener(
			uiEvent -> uiEvent.getUI().addAfterNavigationListener(
				navEvent -> ActionRegistry.getCurrent().updateActions(navEvent.getActiveChain())));
	}
}

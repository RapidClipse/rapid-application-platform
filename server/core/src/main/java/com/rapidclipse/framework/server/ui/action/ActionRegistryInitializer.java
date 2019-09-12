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
package com.rapidclipse.framework.server.ui.action;

import java.lang.reflect.Modifier;
import java.util.Set;

import javax.servlet.ServletContainerInitializer;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.HandlesTypes;

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

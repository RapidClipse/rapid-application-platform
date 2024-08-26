/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

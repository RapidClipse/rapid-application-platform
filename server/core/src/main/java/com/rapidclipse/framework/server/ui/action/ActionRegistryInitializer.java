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

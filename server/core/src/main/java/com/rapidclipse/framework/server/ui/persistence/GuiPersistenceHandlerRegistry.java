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

package com.rapidclipse.framework.server.ui.persistence;

import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.server.ui.persistence.handler.AbstractFieldHandler;
import com.rapidclipse.framework.server.ui.persistence.handler.TabsHandler;
import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.component.Component;


public final class GuiPersistenceHandlerRegistry
{
	private static GuiPersistenceHandlerRegistry INSTANCE;
	
	public static GuiPersistenceHandlerRegistry getInstance()
	{
		if(INSTANCE == null)
		{
			INSTANCE = new GuiPersistenceHandlerRegistry();
		}
		return INSTANCE;
	}
	
	private final Map<Class<? extends Component>, GuiPersistenceHandler<? extends Component>> handlers;
	
	private GuiPersistenceHandlerRegistry()
	{
		this.handlers = new HashMap<>();
		addDefaultHandlers();
		addProvidedHandlers();
	}
	
	private void addDefaultHandlers()
	{
		registerHandler(new AbstractFieldHandler<>());
		registerHandler(new TabsHandler());
	}
	
	private void addProvidedHandlers()
	{
		ServiceLoader.forType(GuiPersistenceHandlerProvider.class).servicesUncached()
			.forEach(provider -> provider.registerHandlers(this));
	}
	
	public <C extends Component> void registerHandler(final GuiPersistenceHandler<C> handler)
	{
		this.registerHandler(handler.handledType(), handler);
	}
	
	public <C extends Component> void registerHandler(
		final Class<C> type,
		final GuiPersistenceHandler<? super C> handler)
	{
		this.handlers.put(type, handler);
	}
	
	@SuppressWarnings("unchecked")
	public <C extends Component> GuiPersistenceHandler<? super C> lookupHandler(final C component)
	{
		return (GuiPersistenceHandler<? super C>)lookupHandler(component.getClass());
	}
	
	@SuppressWarnings("unchecked") // cast necessary due to
	// type-heterogeneous collection content
	public <C extends Component> GuiPersistenceHandler<? super C> lookupHandler(
		final Class<C> componentType)
	{
		// check for a handler directly fitting the passed type
		final GuiPersistenceHandler<? extends Component> handler = this.handlers.get(componentType);
		if(handler != null)
		{
			return (GuiPersistenceHandler<? super C>)handler;
		}
		
		final Class<?> superclass = componentType.getSuperclass();
		if(superclass != null && Component.class.isAssignableFrom(superclass))
		{
			return (GuiPersistenceHandler<? super C>)lookupHandler(
				(Class<? extends Component>)superclass);
		}
		
		/*
		 * potentially null handler returned intentionally to give calling
		 * context the decision to either throw a specific exception or get a
		 * handler from somewhere else. This provider's answer is just
		 * "no handler found" (null), that is not an exception/problem.
		 */
		return (GuiPersistenceHandler<? super C>)handler;
	}
}

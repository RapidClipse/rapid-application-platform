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

package software.xdev.rap.server.ui.persistence;


import java.util.HashMap;
import java.util.Map;

import com.vaadin.flow.component.Component;

import software.xdev.rap.server.ui.persistence.handler.AbstractFieldHandler;
import software.xdev.rap.server.ui.persistence.handler.TabsHandler;
import software.xdev.rap.server.util.ServiceLoader;


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
		this.registerHandler(handler.handledType(),handler);
	}
	
	
	public <C extends Component> void registerHandler(final Class<C> type,
			final GuiPersistenceHandler<? super C> handler)
	{
		this.handlers.put(type,handler);
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

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.persistence;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import org.rapidpm.dependencies.core.logger.HasLogger;

import com.vaadin.flow.component.Component;


public interface GuiPersister
{
	public GuiPersistentStates persistState();
	
	public void restoreState(GuiPersistentStates states);
	
	public static GuiPersister New(final Map<String, Map<String, Component>> entries)
	{
		return new Default(entries);
	}
	
	public static class Default implements GuiPersister, HasLogger
	{
		private final Map<String, Map<String, Component>> entries;
		
		protected Default(final Map<String, Map<String, Component>> entries)
		{
			super();
			this.entries = entries;
		}
		
		@Override
		public GuiPersistentStates persistState()
		{
			final Map<String, GuiPersistentState> states = new HashMap<>();
			
			this.entries.forEach((name, componentMap) -> {
				final Map<String, GuiPersistenceEntry> entryMap = componentMap.entrySet().stream()
					.collect(Collectors.toMap(e -> e.getKey(),
						e -> persistComponent(e.getValue())));
				states.put(name, GuiPersistentState.New(name, entryMap));
			});
			
			return GuiPersistentStates.New(states);
		}
		
		protected GuiPersistenceEntry persistComponent(final Component component)
		{
			return GuiPersistenceHandlerRegistry.getInstance().lookupHandler(component)
				.persist(component);
		}
		
		@Override
		public void restoreState(final GuiPersistentStates states)
		{
			states.states().forEach((name, state) -> {
				final Map<String, Component> componentMap = this.entries.get(name);
				if(componentMap == null)
				{
					logger().info("[GuiPersister.restoreState] Unused state: " + name);
				}
				else
				{
					state.entries().forEach((identifier, entry) -> {
						final Component component = componentMap.get(identifier);
						if(component == null)
						{
							logger().info("[GuiPersister.restoreState] Component not found: " + name);
						}
						else
						{
							GuiPersistenceHandlerRegistry.getInstance().lookupHandler(component)
								.restore(component, entry);
						}
					});
				}
			});
		}
	}
}

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.persistence;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.ui.UIUtils;
import com.vaadin.flow.component.Component;


public interface GuiPersistenceAnalyzer
{
	public default GuiPersister createPersister(final String name, final Component root)
	{
		final Map<String, Component> map = new HashMap<>();
		map.put(name, root);
		return createPersister(map);
	}
	
	public GuiPersister createPersister(Map<String, Component> roots);
	
	public static GuiPersistenceAnalyzer New()
	{
		return new Default();
	}
	
	public static class Default implements GuiPersistenceAnalyzer
	{
		protected Default()
		{
			super();
		}
		
		@Override
		public GuiPersister createPersister(final Map<String, Component> roots)
		{
			final Map<String, Map<String, Component>> entries = new HashMap<>();
			roots.entrySet().forEach(entry -> {
				final String                 name = entry.getKey();
				final Component              root = entry.getValue();
				final Map<String, Component> map  = collectComponents(root).stream()
					.collect(Collectors.toMap(c -> getIdentifier(c), c -> c));
				entries.put(name, map);
			});
			
			return GuiPersister.New(entries);
		}
		
		private String getIdentifier(final Component component)
		{
			final String identifier = componentIdentifier(component);
			if(identifier == null || identifier.length() == 0)
			{
				throw new IllegalArgumentException("Empty identifier");
			}
			return identifier;
		}
		
		protected boolean persist(final Component component)
		{
			if(GuiPersistenceHandlerRegistry.getInstance().lookupHandler(component) == null)
			{
				return false;
			}
			if(!PersistFlag.get(component))
			{
				return false;
			}
			final String identifier = componentIdentifier(component);
			return identifier != null && identifier.length() > 0;
		}
		
		protected String componentIdentifier(final Component component)
		{
			return component.getId().orElse(null);
		}
		
		protected Collection<Component> collectComponents(final Component root)
		{
			final Collection<Component> collection = new HashSet<>();
			UIUtils.traverseComponentTree(root, cpn -> {
				if(persist(cpn))
				{
					collection.add(cpn);
				}
			});
			return collection;
		}
	}
}

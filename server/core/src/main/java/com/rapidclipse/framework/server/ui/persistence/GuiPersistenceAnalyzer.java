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
		return new Implementation();
	}
	
	public static class Implementation implements GuiPersistenceAnalyzer
	{
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
			UIUtils.lookupComponentTree(root, cpn -> {
				if(persist(cpn))
				{
					collection.add(cpn);
				}
				return null;
			});
			return collection;
		}
	}
}

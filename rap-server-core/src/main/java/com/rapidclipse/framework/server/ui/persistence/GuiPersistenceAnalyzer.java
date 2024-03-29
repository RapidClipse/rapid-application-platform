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

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
package com.rapidclipse.framework.server.ui.persistence.handler;

import java.util.Map;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.vaadin.flow.component.tabs.Tabs;


public class TabsHandler extends ComponentHandler<Tabs>
{
	protected static final String SELECTED_TAB_INDEX = "selectedTabIndex";
	
	public TabsHandler()
	{
		super();
	}

	@Override
	public Class<Tabs> handledType()
	{
		return Tabs.class;
	}
	
	@Override
	protected void addEntryValues(final Map<String, Object> entryValues, final Tabs component)
	{
		super.addEntryValues(entryValues, component);
		
		entryValues.put(SELECTED_TAB_INDEX, component.getSelectedIndex());
	}
	
	@Override
	public void restore(final Tabs component, final GuiPersistenceEntry entry)
	{
		super.restore(component, entry);
		
		component.setSelectedIndex(((Number)entry.value(SELECTED_TAB_INDEX)).intValue());
	}
}

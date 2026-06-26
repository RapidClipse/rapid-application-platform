/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

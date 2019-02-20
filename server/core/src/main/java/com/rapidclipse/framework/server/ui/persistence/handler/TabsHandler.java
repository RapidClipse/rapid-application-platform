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

package com.rapidclipse.framework.server.ui.persistence.handler;

import java.util.Map;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.vaadin.flow.component.tabs.Tabs;


public class TabsHandler extends ComponentHandler<Tabs>
{
	protected static final String SELECTED_TAB_INDEX = "selectedTabIndex";
	
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
		
		component.setSelectedIndex(number(entry.value(SELECTED_TAB_INDEX)).intValue());
	}
}

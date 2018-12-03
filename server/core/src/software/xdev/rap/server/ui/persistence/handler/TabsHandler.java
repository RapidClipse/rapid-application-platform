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

package software.xdev.rap.server.ui.persistence.handler;


import java.util.Map;

import com.vaadin.flow.component.tabs.Tabs;

import software.xdev.rap.server.ui.persistence.GuiPersistenceEntry;


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
		super.addEntryValues(entryValues,component);
		
		entryValues.put(SELECTED_TAB_INDEX,component.getSelectedIndex());
	}
	
	
	@Override
	public void restore(final Tabs component, final GuiPersistenceEntry entry)
	{
		super.restore(component,entry);
		
		component.setSelectedIndex(number(entry.value(SELECTED_TAB_INDEX)).intValue());
	}
}

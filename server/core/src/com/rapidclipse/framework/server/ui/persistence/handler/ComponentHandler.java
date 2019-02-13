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

package com.rapidclipse.framework.server.ui.persistence.handler;


import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceHandler;
import com.vaadin.flow.component.Component;


public abstract class ComponentHandler<C extends Component> implements GuiPersistenceHandler<C>
{
	protected static final String KEY_VISIBLE = "visible";
	
	
	/**
	 * Trivial utility method to improve use site readability.
	 *
	 * @param uncastNumberInstance
	 *            the uncast {@link Number} instance to be cast.
	 * @return the instance cast as {@link Number}.
	 */
	protected static Number number(final Object uncastNumberInstance)
	{
		return (Number)uncastNumberInstance;
	}
	
	
	@Override
	public GuiPersistenceEntry persist(final C component)
	{
		final Map<String, Object> valueTable = new HashMap<>();
		this.addEntryValues(valueTable,component);
		return GuiPersistenceEntry.New(valueTable);
	}
	
	
	protected void addEntryValues(final Map<String, Object> entryValues, final C component)
	{
		entryValues.put(KEY_VISIBLE,component.isVisible());
	}
	
	
	@Override
	public void restore(final C component, final GuiPersistenceEntry entry)
	{
		component.setVisible((Boolean)entry.value(KEY_VISIBLE));
	}
}

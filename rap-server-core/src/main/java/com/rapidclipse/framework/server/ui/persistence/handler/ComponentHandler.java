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

import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceHandler;
import com.vaadin.flow.component.Component;


public abstract class ComponentHandler<C extends Component> implements GuiPersistenceHandler<C>
{
	protected static final String KEY_VISIBLE = "visible";

	protected ComponentHandler()
	{
		super();
	}

	@Override
	public GuiPersistenceEntry persist(final C component)
	{
		final Map<String, Object> valueTable = new HashMap<>();
		this.addEntryValues(valueTable, component);
		return GuiPersistenceEntry.New(valueTable);
	}
	
	protected void addEntryValues(final Map<String, Object> entryValues, final C component)
	{
		entryValues.put(KEY_VISIBLE, component.isVisible());
	}
	
	@Override
	public void restore(final C component, final GuiPersistenceEntry entry)
	{
		component.setVisible((Boolean)entry.value(KEY_VISIBLE));
	}
}

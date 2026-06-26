/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

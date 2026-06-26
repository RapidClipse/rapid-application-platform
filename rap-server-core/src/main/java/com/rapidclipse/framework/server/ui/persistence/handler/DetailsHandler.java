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
import com.vaadin.flow.component.details.Details;


public class DetailsHandler extends ComponentHandler<Details>
{
	protected static final String OPENED = "opened";
	
	public DetailsHandler()
	{
		super();
	}

	@Override
	public Class<Details> handledType()
	{
		return Details.class;
	}
	
	@Override
	protected void addEntryValues(final Map<String, Object> entryValues, final Details component)
	{
		super.addEntryValues(entryValues, component);
		
		entryValues.put(OPENED, component.isOpened());
	}
	
	@Override
	public void restore(final Details component, final GuiPersistenceEntry entry)
	{
		super.restore(component, entry);
		
		component.setOpened(((Boolean)entry.value(OPENED)).booleanValue());
	}
}

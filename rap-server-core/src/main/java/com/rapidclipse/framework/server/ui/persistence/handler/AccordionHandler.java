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
import java.util.OptionalInt;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.vaadin.flow.component.accordion.Accordion;


public class AccordionHandler extends ComponentHandler<Accordion>
{
	protected static final String OPENED_INDEX = "openedIndex";

	public AccordionHandler()
	{
		super();
	}

	@Override
	public Class<Accordion> handledType()
	{
		return Accordion.class;
	}

	@Override
	protected void addEntryValues(final Map<String, Object> entryValues, final Accordion component)
	{
		super.addEntryValues(entryValues, component);

		final OptionalInt openedIndex = component.getOpenedIndex();
		if(openedIndex.isPresent())
		{
			entryValues.put(OPENED_INDEX, openedIndex.getAsInt());
		}
	}

	@Override
	public void restore(final Accordion component, final GuiPersistenceEntry entry)
	{
		super.restore(component, entry);

		final Object openedIndex = entry.value(OPENED_INDEX);
		if(openedIndex != null)
		{
			component.open(((Number)entry.value(OPENED_INDEX)).intValue());
		}
	}
}

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
import java.util.OptionalInt;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.vaadin.flow.component.accordion.Accordion;


public class AccordionHandler extends ComponentHandler<Accordion>
{
	protected static final String OPENED_INDEX = "openedIndex";
	
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

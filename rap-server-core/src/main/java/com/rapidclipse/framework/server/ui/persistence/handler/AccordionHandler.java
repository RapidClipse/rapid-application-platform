/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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

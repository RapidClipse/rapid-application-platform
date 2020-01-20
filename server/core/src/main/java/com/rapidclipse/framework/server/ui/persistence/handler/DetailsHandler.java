/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.persistence.handler;

import java.util.Map;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.vaadin.flow.component.details.Details;


public class DetailsHandler extends ComponentHandler<Details>
{
	protected static final String OPENED = "opened";

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

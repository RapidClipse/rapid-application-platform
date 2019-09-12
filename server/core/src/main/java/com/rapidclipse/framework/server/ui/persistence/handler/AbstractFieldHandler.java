/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.persistence.handler;

import java.util.Map;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.rapidclipse.framework.server.ui.persistence.PersistValueFlag;
import com.vaadin.flow.component.AbstractField;


@SuppressWarnings("rawtypes")
public class AbstractFieldHandler<C extends AbstractField> extends ComponentHandler<C>
	implements HasValueHandler
{
	@SuppressWarnings("unchecked")
	@Override
	public Class<C> handledType()
	{
		return (Class<C>)AbstractField.class;
	}

	@Override
	protected void addEntryValues(final Map<String, Object> entryValues, final C component)
	{
		super.addEntryValues(entryValues, component);

		if(PersistValueFlag.get(component))
		{
			entryValues.put(VALUE, getFieldValueToStore(component.getValue()));
		}
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void restore(final C component, final GuiPersistenceEntry entry)
	{
		super.restore(component, entry);

		if(PersistValueFlag.get(component))
		{
			component.setValue(getFieldValueToRestore(entry.value(VALUE)));
		}
	}
}

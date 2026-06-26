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
import com.rapidclipse.framework.server.ui.persistence.PersistValueFlag;
import com.vaadin.flow.component.AbstractField;


@SuppressWarnings("rawtypes")
public class AbstractFieldHandler<C extends AbstractField> extends ComponentHandler<C>
	implements HasValueHandler
{
	public AbstractFieldHandler()
	{
		super();
	}

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

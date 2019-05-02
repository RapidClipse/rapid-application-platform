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

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.rapidclipse.framework.server.ui.persistence.PersistValueFlag;
import com.vaadin.flow.component.AbstractCompositeField;


@SuppressWarnings("rawtypes")
public class AbstractCompositeFieldHandler<C extends AbstractCompositeField> extends ComponentHandler<C>
	implements HasValueHandler
{
	@SuppressWarnings("unchecked")
	@Override
	public Class<C> handledType()
	{
		return (Class<C>)AbstractCompositeField.class;
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

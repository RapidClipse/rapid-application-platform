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

import java.lang.reflect.Array;
import java.util.Map;

import com.rapidclipse.framework.server.data.ValueTransfer;
import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.rapidclipse.framework.server.ui.persistence.PersistValueFlag;
import com.vaadin.flow.component.AbstractField;


@SuppressWarnings("rawtypes")
public class AbstractFieldHandler<C extends AbstractField> extends ComponentHandler<C>
{
	protected static final String KEY_VALUE = "value";
	
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
			entryValues.put(KEY_VALUE, getFieldValueToStore(component.getValue()));
		}
	}
	
	protected Object getFieldValueToStore(final Object value)
	{
		if(value == null)
		{
			return null;
		}
		
		final Class<? extends Object> clazz = value.getClass();
		if(clazz.isArray())
		{
			final int      length = Array.getLength(value);
			final Object[] array  = new Object[length];
			for(int i = 0; i < array.length; i++)
			{
				array[i] = getFieldValueToStore(Array.get(value, i));
			}
			return array;
		}
		
		return ValueTransfer.put(value);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void restore(final C component, final GuiPersistenceEntry entry)
	{
		super.restore(component, entry);
		
		if(PersistValueFlag.get(component))
		{
			component.setValue(getFieldValueToRestore(component, entry.value(KEY_VALUE)));
		}
	}
	
	protected Object getFieldValueToRestore(final C component, final Object value)
	{
		if(value == null)
		{
			return null;
		}
		
		final Class<? extends Object> clazz = value.getClass();
		if(clazz.isArray())
		{
			final int      length = Array.getLength(value);
			final Object[] array  = new Object[length];
			for(int i = 0; i < array.length; i++)
			{
				array[i] = getFieldValueToRestore(component, Array.get(value, i));
			}
			return array;
		}
		
		return ValueTransfer.get(value);
	}
}

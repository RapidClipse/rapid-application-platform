/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.persistence.handler;


import java.lang.reflect.Array;
import java.util.Map;

import com.vaadin.flow.component.AbstractField;

import software.xdev.rap.server.data.ValueTransfer;
import software.xdev.rap.server.ui.persistence.GuiPersistenceEntry;
import software.xdev.rap.server.ui.persistence.PersistValueFlag;


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
		super.addEntryValues(entryValues,component);
		
		if(PersistValueFlag.get(component))
		{
			entryValues.put(KEY_VALUE,getFieldValueToStore(component.getValue()));
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
			final int length = Array.getLength(value);
			final Object[] array = new Object[length];
			for(int i = 0; i < array.length; i++)
			{
				array[i] = getFieldValueToStore(Array.get(value,i));
			}
			return array;
		}

		return ValueTransfer.put(value);
	}
	
	
	@SuppressWarnings("unchecked")
	@Override
	public void restore(final C component, final GuiPersistenceEntry entry)
	{
		super.restore(component,entry);
		
		if(PersistValueFlag.get(component))
		{
			component.setValue(getFieldValueToRestore(component,entry.value(KEY_VALUE)));
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
			final int length = Array.getLength(value);
			final Object[] array = new Object[length];
			for(int i = 0; i < array.length; i++)
			{
				array[i] = getFieldValueToRestore(component,Array.get(value,i));
			}
			return array;
		}

		return ValueTransfer.get(value);
	}
}

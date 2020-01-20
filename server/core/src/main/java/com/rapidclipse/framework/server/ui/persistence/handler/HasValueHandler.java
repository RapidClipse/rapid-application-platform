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

import java.lang.reflect.Array;

import com.rapidclipse.framework.server.data.ValueTransfer;


/**
 * @author XDEV Software
 *
 */
public interface HasValueHandler
{
	public static final String VALUE = "value";

	public default Object getFieldValueToStore(final Object value)
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
	
	public default Object getFieldValueToRestore(final Object value)
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
				array[i] = getFieldValueToRestore(Array.get(value, i));
			}
			return array;
		}
		
		return ValueTransfer.get(value);
	}
}

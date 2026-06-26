/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

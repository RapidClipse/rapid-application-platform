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
package com.rapidclipse.framework.server.util;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.Arrays;


/**
 * @author XDEV Software
 */
public final class BeanInfoUtils
{
	public static PropertyDescriptor getPropertyDescriptor(
		Class<?> beanClass,
		final String propertyPath)
	{
		try
		{
			BeanInfo beanInfo = Introspector.getBeanInfo(beanClass);
			
			final String[] parts = propertyPath.split("\\.");
			for(int i = 0; i < parts.length - 1; i++)
			{
				final String             name               = parts[i];
				final PropertyDescriptor propertyDescriptor = getPropertyDescriptor(beanInfo, name);
				if(propertyDescriptor == null)
				{
					return null;
				}
				beanClass = propertyDescriptor.getPropertyType();
				if(beanClass == null)
				{
					return null;
				}
				beanInfo = Introspector.getBeanInfo(beanClass);
			}
			
			return getPropertyDescriptor(beanInfo, parts[parts.length - 1]);
		}
		catch(final Exception e)
		{
			throw new RuntimeException(e);
		}
	}
	
	public static PropertyDescriptor getPropertyDescriptor(
		final BeanInfo beanInfo,
		final String name)
	{
		return Arrays.stream(beanInfo.getPropertyDescriptors())
			.filter(d -> d.getName().equals(name)).findFirst().orElse(null);
	}
	
	private BeanInfoUtils()
	{
		throw new Error();
	}
}

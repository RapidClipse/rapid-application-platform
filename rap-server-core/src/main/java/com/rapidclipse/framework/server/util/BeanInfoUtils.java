/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

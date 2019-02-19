/*-
 * ---
 * Rapid Application Platform / Server / Core
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
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
			BeanInfo       beanInfo = Introspector.getBeanInfo(beanClass);
			
			final String[] parts    = propertyPath.split("\\.");
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

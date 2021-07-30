/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.resources;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.Arrays;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface CaptionParameterProvider
{
	public String getParameterValue(Object element, String parameterName);
	
	public static class SimpleCaptionParameterProvider implements CaptionParameterProvider
	{
		/**
		 *
		 */
		public SimpleCaptionParameterProvider()
		{
			super();
		}
		
		@Override
		public String getParameterValue(final Object element, final String parameterName)
		{
			return parameterName;
		}
	}
	
	public static class BeanInfoCaptionParameterProvider implements CaptionParameterProvider
	{
		/**
		 *
		 */
		public BeanInfoCaptionParameterProvider()
		{
			super();
		}
		
		@Override
		public String getParameterValue(final Object element, final String parameterName)
		{
			try
			{
				final BeanInfo beanInfo = Introspector.getBeanInfo(element.getClass());
				
				final PropertyDescriptor propertyDescriptor = Arrays
					.stream(beanInfo.getPropertyDescriptors())
					.filter(pd -> pd.getName().equals(parameterName)).findFirst().orElse(null);
				if(propertyDescriptor == null)
				{
					return parameterName;
				}
				
				final Method method = propertyDescriptor.getReadMethod();
				if(method.getParameterCount() > 0)
				{
					return parameterName;
				}
				
				try
				{
					final Object value = method.invoke(element);
					return String.valueOf(value);
				}
				catch(final Throwable t)
				{
					return parameterName;
				}
			}
			catch(final IntrospectionException e)
			{
			}
			
			return parameterName;
		}
	}
}

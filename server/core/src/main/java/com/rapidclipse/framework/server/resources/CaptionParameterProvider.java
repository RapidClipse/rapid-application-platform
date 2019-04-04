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

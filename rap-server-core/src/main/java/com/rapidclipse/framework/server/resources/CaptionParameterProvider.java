/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

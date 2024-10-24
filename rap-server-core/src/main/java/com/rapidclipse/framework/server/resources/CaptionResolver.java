/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.resources;

import static java.util.Objects.requireNonNull;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Locale;
import java.util.Objects;
import java.util.function.Function;

import com.rapidclipse.framework.server.util.ServiceLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * @author XDEV Software
 *
 */
public interface CaptionResolver
{
	public String resolveCaption(Object element, Locale locale);

	public String resolveCaption(Object element, String captionValue, Locale locale);
	
	public static CaptionResolver New()
	{
		return new Default("");
	}
	
	/**
	 * @since 11.00.00
	 */
	public static CaptionResolver New(final String nullRepresentation)
	{
		return new Default(requireNonNull(nullRepresentation));
	}

	public static class Default implements CaptionResolver
	{
		private final String nullRepresentation;
		
		protected Default(final String nullRepresentation)
		{
			super();
			
			this.nullRepresentation = nullRepresentation;
		}

		@Override
		public String resolveCaption(final Object element, final Locale locale)
		{
			if(element == null)
			{
				return this.nullRepresentation;
			}
			
			final Caption caption = getCaptionAnnotation(element);
			final String  value   = caption != null ? caption.value() : null;
			if(value == null || value.length() == 0)
			{
				if(element instanceof Class)
				{
					return ((Class<?>)element).getSimpleName();
				}
				if(element instanceof Member)
				{
					return ((Member)element).getName();
				}

				// avoid stack overflow
				if(!isCallFromToString())
				{
					return element.toString();
				}

				return element.getClass().getName() + "@" + Integer.toHexString(element.hashCode());
			}

			return resolveCaption(element, value, locale);
		}

		@Override
		public String resolveCaption(
			final Object element,
			final String captionValue,
			final Locale locale)
		{
			final String caption = StringResourceUtils.localizeString(captionValue, locale, element);
			return format(caption, element);
		}

		protected Caption getCaptionAnnotation(final Object element)
		{
			AnnotatedElement annotatedElement;
			if(element instanceof AnnotatedElement)
			{
				annotatedElement = (AnnotatedElement)element;
			}
			else
			{
				annotatedElement = element.getClass();
			}
			return annotatedElement.getAnnotation(Caption.class);
		}

		protected boolean isCallFromToString()
		{
			final Throwable throwable = new Throwable();
			throwable.fillInStackTrace();
			return Arrays.stream(throwable.getStackTrace())
				.filter(element -> "toString".equals(element.getMethodName())).findFirst()
				.isPresent();
		}

		protected String format(String string, final Object element)
		{
			CaptionParameterProvider parameterProvider = null;

			int                      start;
			int                      searchStart       = 0;
			while((start = string.indexOf("{%", searchStart)) >= 0)
			{
				final int end = string.indexOf("}", start + 2);
				if(end > start)
				{
					if(parameterProvider == null)
					{
						parameterProvider = getParameterProvider(element);
					}

					final String        parameterName = string.substring(start + 2, end);
					final String        value         = parameterProvider.getParameterValue(element, parameterName);

					final StringBuilder sb            = new StringBuilder();
					sb.append(string.substring(0, start));
					sb.append(value);
					sb.append(string.substring(end + 1));
					string      = sb.toString();

					searchStart = start + value.length();
				}
				else
				{
					break;
				}
			}
			return string;
		}

		protected CaptionParameterProvider getParameterProvider(final Object element)
		{
			return ServiceLoader.forType(CaptionParameterProviderFactory.class).servicesStream()
				.map(factory -> factory.getParameterProvider(element)).filter(Objects::nonNull)
				.findFirst().orElse(null);
		}
	}

	public static class BeanInfoParameterProvider implements Function<String, String>
	{
		private static final Logger LOG = LoggerFactory.getLogger(BeanInfoParameterProvider.class);
		private final Object element;

		private boolean      acquireBeanInfo = true;
		private BeanInfo     beanInfo;

		public BeanInfoParameterProvider(final Object element)
		{
			this.element = element;
		}

		protected Object getElement()
		{
			return this.element;
		}

		@Override
		public String apply(final String parameter)
		{
			if(this.acquireBeanInfo)
			{
				this.acquireBeanInfo = false;

				try
				{
					this.beanInfo = Introspector.getBeanInfo(this.element.getClass());
				}
				catch(final IntrospectionException e)
				{
					LOG.error(e.getMessage(), e);
				}
			}

			if(this.beanInfo == null)
			{
				return parameter;
			}

			return getParameter(parameter, this.beanInfo);
		}

		protected String getParameter(final String parameter, final BeanInfo beanInfo)
		{
			final PropertyDescriptor propertyDescriptor = Arrays
				.stream(beanInfo.getPropertyDescriptors())
				.filter(pd -> pd.getName().equals(parameter)).findFirst().orElse(null);
			if(propertyDescriptor == null)
			{
				return parameter;
			}

			final Method method = propertyDescriptor.getReadMethod();
			if(method.getParameterCount() > 0)
			{
				return parameter;
			}

			try
			{
				final Object value = method.invoke(this.element);
				return String.valueOf(value);
			}
			catch(final Throwable t)
			{
				LOG.error(t.getMessage(), t);
				return parameter;
			}
		}
	}
}

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

package com.rapidclipse.framework.server.ui.filter;

import static java.util.stream.Collectors.toList;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.List;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.rapidclipse.framework.server.util.ServicePriority;


/**
 * @author XDEV Software
 *
 */
@ServicePriority(ServicePriority.MIN)
public class PojoFilterSubjectFactory implements FilterSubjectFactory
{
	@Override
	public boolean supports(final Object source)
	{
		return true;
	}
	
	@Override
	public FilterSubject createFilterSubject(final Object source)
	{
		try
		{
			final Class<?> clazz =
				source instanceof Class ? (Class<?>)source : source.getClass();
			
			final PropertyDescriptor[] propertyDescriptors = Introspector.getBeanInfo(clazz)
				.getPropertyDescriptors();
			
			final List<FilterProperty<?>> searchableProperties = Arrays.stream(propertyDescriptors)
				.filter(d -> String.class.equals(d.getPropertyType()))
				.map(d -> toFilterProperty(clazz, d)).collect(toList());
			
			final List<FilterProperty<?>> filterableProperties = Arrays.stream(propertyDescriptors)
				.filter(d -> Comparable.class.isAssignableFrom(d.getPropertyType()))
				.map(d -> toFilterProperty(clazz, d)).collect(toList());
			
			return FilterSubject.New(searchableProperties, filterableProperties);
		}
		catch(final IntrospectionException e)
		{
			throw new RuntimeException(e);
		}
	}
	
	protected FilterProperty<?> toFilterProperty(
		final Class<?> clazz,
		final PropertyDescriptor beanProperty)
	{
		final String name = beanProperty.getName();
		return FilterProperty.New(name, beanProperty.getPropertyType(),
			CaptionUtils.resolveCaption(clazz, name));
	}
}

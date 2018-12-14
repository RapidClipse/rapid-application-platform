/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.filter;


import static java.util.stream.Collectors.toList;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.List;

import software.xdev.rap.server.resources.CaptionUtils;
import software.xdev.rap.server.util.ServicePriority;


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
			final Class<?> clazz = source instanceof Class ? (Class<?>)source : source.getClass();

			final PropertyDescriptor[] propertyDescriptors = Introspector.getBeanInfo(clazz)
					.getPropertyDescriptors();
			
			final List<FilterProperty<?>> searchableProperties = Arrays.stream(propertyDescriptors)
					.filter(d -> String.class.equals(d.getPropertyType()))
					.map(d -> toFilterProperty(clazz,d)).collect(toList());

			final List<FilterProperty<?>> filterableProperties = Arrays.stream(propertyDescriptors)
					.filter(d -> Comparable.class.isAssignableFrom(d.getPropertyType()))
					.map(d -> toFilterProperty(clazz,d)).collect(toList());

			return FilterSubject.New(searchableProperties,filterableProperties);
		}
		catch(final IntrospectionException e)
		{
			throw new RuntimeException(e);
		}
	}
	
	
	protected FilterProperty<?> toFilterProperty(final Class<?> clazz,
			final PropertyDescriptor beanProperty)
	{
		final String name = beanProperty.getName();
		return FilterProperty.New(name,beanProperty.getPropertyType(),
				CaptionUtils.resolveCaption(clazz,name));
	}
}

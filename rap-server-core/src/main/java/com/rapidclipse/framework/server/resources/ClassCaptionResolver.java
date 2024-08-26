/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.resources;

import java.beans.PropertyDescriptor;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;

import com.rapidclipse.framework.server.util.BeanInfoUtils;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface ClassCaptionResolver
{
	public String resolveCaption(final Class<?> clazz, final String propertyName);
	
	public static class BeanClassCaptionResolver implements ClassCaptionResolver
	{
		/**
		 *
		 */
		public BeanClassCaptionResolver()
		{
			super();
		}
		
		@Override
		public String resolveCaption(final Class<?> clazz, final String propertyName)
		{
			final PropertyDescriptor propertyDescriptor = BeanInfoUtils.getPropertyDescriptor(clazz,
				propertyName);
			if(propertyDescriptor == null)
			{
				return propertyName;
			}
			
			final Member javaMember = propertyDescriptor.getReadMethod();
			if(javaMember instanceof AnnotatedElement
				&& CaptionUtils.hasCaptionAnnotationValue((AnnotatedElement)javaMember))
			{
				return CaptionUtils.resolveCaption(javaMember);
			}
			
			return propertyDescriptor.getDisplayName();
		}
	}
}

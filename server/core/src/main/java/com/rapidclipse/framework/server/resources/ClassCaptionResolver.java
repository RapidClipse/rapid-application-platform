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

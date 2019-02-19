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

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;


public final class ReflectionUtils
{
	public static <T extends Annotation> Field getAnnotatedField(
		final Class<?> ownerClass,
		final Class<T> annotationClass)
	{
		for(final Field field : ownerClass.getDeclaredFields())
		{
			final T a = field.getDeclaredAnnotation(annotationClass);
			if(a != null)
			{
				return field;
			}
		}

		return null;
	}
	
	public static Object getMemberValue(final Object obj, final Member member)
	{
		if(member instanceof Field)
		{
			final Field   field      = (Field)member;
			final boolean accessible = field.isAccessible();
			try
			{
				field.setAccessible(true);
				try
				{
					return field.get(obj);
				}
				catch(IllegalArgumentException | IllegalAccessException e)
				{
					throw new RuntimeException(e);
				}
			}
			finally
			{
				field.setAccessible(accessible);
			}
		}
		else if(member instanceof Method)
		{
			final Method  method     = (Method)member;
			final boolean accessible = method.isAccessible();
			try
			{
				method.setAccessible(true);
				try
				{
					return method.invoke(obj);
				}
				catch(IllegalAccessException | IllegalArgumentException
					| InvocationTargetException e)
				{
					throw new RuntimeException(e);
				}
			}
			finally
			{
				method.setAccessible(accessible);
			}
		}

		return null;
	}
	
	private ReflectionUtils()
	{
		throw new Error();
	}
}

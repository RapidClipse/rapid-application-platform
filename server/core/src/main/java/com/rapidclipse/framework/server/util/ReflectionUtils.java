/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.util;

import java.lang.annotation.Annotation;
import java.lang.annotation.Inherited;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;


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

	public static <A extends Annotation> boolean
		isAnnotationPresent(final AnnotatedElement annotatedElement, final Class<A> annotationType)
	{
		return findAnnotation(annotatedElement, annotationType) != null;
	}

	public static <A extends Annotation> A
		findAnnotation(final AnnotatedElement annotatedElement, final Class<A> annotationType)
	{
		return Optional.ofNullable(
			// Find direct
			annotatedElement.getAnnotation(annotationType))
			.orElse(
				// Find annotations of annotation (sort of annotation inheritance)
				Arrays.stream(annotatedElement.getAnnotations())
					// Avoid stack overflows with base annotations
					.filter(a -> !isBaseAnnotation(annotatedElement))
					.map(a -> findAnnotation(a.annotationType(), annotationType))
					.filter(Objects::nonNull).findFirst()
					.orElse(
						// Find inherited if @Inherited is present
						annotatedElement instanceof Class<?>
							&& ((Class<?>)annotatedElement).getSuperclass() != null
							&& annotationType.isAnnotationPresent(Inherited.class)
								? findAnnotation(((Class<?>)annotatedElement).getSuperclass(), annotationType)
								: null));
	}

	private static boolean isBaseAnnotation(final AnnotatedElement annotatedElement)
	{
		if(annotatedElement instanceof Class)
		{
			final Class<?> clazz = (Class<?>)annotatedElement;
			return clazz.isAnnotation() && clazz.getName().startsWith("java.lang.");
		}
		return false;
	}
	
	public static boolean hasDefaultConstructor(final Class<?> clazz)
	{
		return getDefaultConstructor(clazz) != null;
	}
	
	public static <T> Constructor<T> getDefaultConstructor(final Class<T> clazz)
	{
		try
		{
			return clazz.getDeclaredConstructor();
		}
		catch(final SecurityException e)
		{
			return null;
		}
		catch(final NoSuchMethodException e)
		{
			return null;
		}
	}

	private ReflectionUtils()
	{
		throw new Error();
	}
}

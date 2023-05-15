/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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

import java.lang.reflect.AnnotatedElement;
import java.util.Locale;
import java.util.Objects;

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public final class CaptionUtils
{
	private static CaptionResolver captionResolver;

	public static CaptionResolver getCaptionResolver()
	{
		if(captionResolver == null)
		{
			captionResolver = CaptionResolver.New();
		}

		return captionResolver;
	}

	public static void setCaptionResolver(final CaptionResolver captionResolver)
	{
		CaptionUtils.captionResolver = captionResolver;
	}

	public static String resolveCaption(final Object element)
	{
		return resolveCaption(element, (Locale)null);
	}

	public static String resolveCaption(final Object element, final Locale locale)
	{
		return getCaptionResolver().resolveCaption(element, locale);
	}

	public static String resolveCaption(final Object element, final String captionValue)
	{
		return resolveCaption(element, captionValue, null);
	}

	public static String resolveCaption(
		final Object element,
		final String captionValue,
		final Locale locale)
	{
		return getCaptionResolver().resolveCaption(element, captionValue, locale);
	}

	public static String resolveCaption(final Class<?> clazz, final String propertyName)
	{
		return ServiceLoader.forType(ClassCaptionResolverFactory.class).servicesStream()
			.map(factory -> factory.getClassCaptionResolver(clazz)).filter(Objects::nonNull)
			.map(resolver -> resolver.resolveCaption(clazz, propertyName))
			.filter(Objects::nonNull).findFirst().orElse(propertyName);
	}

	public static boolean hasCaptionAnnotationValue(final AnnotatedElement element)
	{
		final Caption annotation = element.getAnnotation(Caption.class);
		return annotation != null && annotation.value().length() > 0;
	}

	private CaptionUtils()
	{
		throw new Error();
	}
}

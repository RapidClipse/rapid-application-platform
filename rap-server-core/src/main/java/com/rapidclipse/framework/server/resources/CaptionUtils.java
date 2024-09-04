/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

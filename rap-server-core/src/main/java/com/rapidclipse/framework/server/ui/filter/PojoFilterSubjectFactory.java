/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.filter;

import java.util.Collection;


/**
 * @author XDEV Software
 *
 */
public class PojoFilterSubjectFactory extends FilterSubjectFactory.Abstract<Object>
{
	public static PojoFilterSubjectFactory New()
	{
		return new PojoFilterSubjectFactory(null, null);
	}

	public static PojoFilterSubjectFactory New(
		final Collection<String> searchableProperties,
		final Collection<String> filterableProperties)
	{
		return new PojoFilterSubjectFactory(
			searchableProperties,
			filterableProperties);
	}

	public static FilterSubject CreateFilterSubject(final Class<?> type)
	{
		return New().createFilterSubjectForType(type);
	}

	public static FilterSubject CreateFilterSubject(
		final Class<?> type,
		final Collection<String> searchableProperties,
		final Collection<String> filterableProperties)
	{
		return New(searchableProperties, filterableProperties).createFilterSubjectForType(type);
	}

	protected PojoFilterSubjectFactory(
		final Collection<String> searchablePropertyNames,
		final Collection<String> filterablePropertyNames)
	{
		super(searchablePropertyNames, filterablePropertyNames);
	}

	@Override
	public FilterSubject createFilterSubject(final Object source)
	{
		final Class<?> type = source instanceof Class ? (Class<?>)source : source.getClass();
		return createFilterSubjectForType(type);
	}
}

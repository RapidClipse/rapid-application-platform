/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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

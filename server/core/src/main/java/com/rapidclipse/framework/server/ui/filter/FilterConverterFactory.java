/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;

import com.rapidclipse.framework.server.data.filter.Comparison.StringComparison;
import com.rapidclipse.framework.server.data.filter.Filter;
import com.vaadin.flow.function.SerializableFunction;


/**
 * @author XDEV Software
 *
 */
public final class FilterConverterFactory
{
	public static SerializableFunction<String, Filter> createStringConverter(final Serializable identifier)
	{
		return createStringConverter(identifier, false);
	}

	public static SerializableFunction<String, Filter> createStringConverter(
		final Serializable identifier,
		final boolean caseSensitive)
	{
		return filter -> {

			String pattern;
			if(filter == null || (pattern = filter.trim()).isEmpty())
			{
				return null;
			}

			final int length = pattern.length();
			if(length > 0 && pattern.charAt(length - 1) != StringComparison.DEFAULT_WILDCARD)
			{
				pattern += StringComparison.DEFAULT_WILDCARD;
			}

			return Filter.StringComparison(identifier, pattern, caseSensitive);
		};
	}

	private FilterConverterFactory()
	{
		throw new Error();
	}
}

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

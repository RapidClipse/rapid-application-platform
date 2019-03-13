/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
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

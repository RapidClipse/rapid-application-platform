/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.filter;


import java.io.Serializable;

import com.vaadin.flow.function.SerializableFunction;

import software.xdev.rap.server.data.filter.Comparison.StringComparison;
import software.xdev.rap.server.data.filter.Filter;


/**
 * @author XDEV Software
 *
 */
public final class FilterConverterFactory
{
	public static SerializableFunction<String, Filter> New(final Serializable identifier)
	{
		return New(identifier,false);
	}


	public static SerializableFunction<String, Filter> New(final Serializable identifier,
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

			return Filter.StringComparison(identifier,pattern,caseSensitive);
		};
	}


	private FilterConverterFactory()
	{
		throw new Error();
	}
}

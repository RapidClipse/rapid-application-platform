/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.data.filter;

import java.io.Serializable;
import java.util.List;

import com.rapidclipse.framework.server.data.filter.Comparison.Equals;
import com.rapidclipse.framework.server.data.filter.Comparison.Greater;
import com.rapidclipse.framework.server.data.filter.Comparison.GreaterEquals;
import com.rapidclipse.framework.server.data.filter.Comparison.Less;
import com.rapidclipse.framework.server.data.filter.Comparison.LessEquals;
import com.rapidclipse.framework.server.data.filter.Comparison.StringComparison;


/**
 * @author XDEV Software
 *
 */
public interface Filter extends Serializable
{
	public default Composite and(final Filter other)
	{
		return Composite.New(Composite.Connector.AND, this, other);
	}

	public default Composite or(final Filter other)
	{
		return Composite.New(Composite.Connector.OR, this, other);
	}

	@SafeVarargs
	public static Composite And(final Filter... filters)
	{
		return Composite.New(Composite.Connector.AND, filters);
	}

	public static Composite And(final List<Filter> filters)
	{
		return Composite.New(Composite.Connector.AND, filters);
	}

	@SafeVarargs
	public static Composite Or(final Filter... filters)
	{
		return Composite.New(Composite.Connector.OR, filters);
	}

	public static Composite Or(final List<Filter> filters)
	{
		return Composite.New(Composite.Connector.OR, filters);
	}

	public static Not Not(final Filter filter)
	{
		return Not.New(filter);
	}

	public static IsNull IsNull(final Object identifier)
	{
		return IsNull.New(identifier);
	}

	public static Equals Equals(final Object identifier, final Object value)
	{
		return Comparison.Equals(identifier, value);
	}

	public static Greater Greater(final Object identifier, final Comparable<?> value)
	{
		return Comparison.Greater(identifier, value);
	}

	public static GreaterEquals GreaterEquals(final Object identifier, final Comparable<?> value)
	{
		return Comparison.GreaterEquals(identifier, value);
	}

	public static Less Less(final Object identifier, final Comparable<?> value)
	{
		return Comparison.Less(identifier, value);
	}

	public static LessEquals LessEquals(final Object identifier, final Comparable<?> value)
	{
		return Comparison.LessEquals(identifier, value);
	}

	public static StringComparison StringComparison(
		final Object identifier,
		final String value,
		final boolean caseSensitive)
	{
		return Comparison.StringComparison(identifier, value, caseSensitive);
	}

	public static StringComparison StringComparison(
		final Object identifier,
		final String value,
		final boolean caseSensitive,
		final List<Character> wildcards)
	{
		return Comparison.StringComparison(identifier, value, caseSensitive, wildcards);
	}

	public static Between Between(
		final Object identifier,
		final Comparable<?> start,
		final Comparable<?> end)
	{
		return Between.New(identifier, start, end);
	}
}

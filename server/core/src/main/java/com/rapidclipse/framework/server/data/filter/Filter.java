/*-
 * ---
 * Rapid Application Platform / Server / Core
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
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
		return new Not.Implementation(filter);
	}
	
	public static IsNull IsNull(final Object identifier)
	{
		return new IsNull.Implementation(identifier);
	}
	
	public static Equals Equals(final Object identifier, final Object value)
	{
		return new Equals.Implementation(identifier, value);
	}
	
	public static Greater Greater(final Object identifier, final Comparable<?> value)
	{
		return new Greater.Implementation(identifier, value);
	}
	
	public static GreaterEquals GreaterEquals(final Object identifier, final Comparable<?> value)
	{
		return new GreaterEquals.Implementation(identifier, value);
	}
	
	public static Less Less(final Object identifier, final Comparable<?> value)
	{
		return new Less.Implementation(identifier, value);
	}
	
	public static LessEquals LessEquals(final Object identifier, final Comparable<?> value)
	{
		return new LessEquals.Implementation(identifier, value);
	}
	
	public static StringComparison StringComparison(
		final Object identifier,
		final String value,
		final boolean caseSensitive)
	{
		return new StringComparison.Implementation(identifier, value, caseSensitive);
	}
	
	public static StringComparison StringComparison(
		final Object identifier,
		final String value,
		final boolean caseSensitive,
		final List<Character> wildcards)
	{
		return new StringComparison.Implementation(identifier, value, caseSensitive, wildcards);
	}
	
	public static Between Between(
		final Object identifier,
		final Comparable<?> start,
		final Comparable<?> end)
	{
		return new Between.Implementation(identifier, start, end);
	}
}
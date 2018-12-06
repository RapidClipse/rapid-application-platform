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

package software.xdev.rap.server.data.filter;


import java.io.Serializable;
import java.util.List;

import software.xdev.rap.server.data.filter.Comparison.Equals;
import software.xdev.rap.server.data.filter.Comparison.Greater;
import software.xdev.rap.server.data.filter.Comparison.GreaterEquals;
import software.xdev.rap.server.data.filter.Comparison.Less;
import software.xdev.rap.server.data.filter.Comparison.LessEquals;
import software.xdev.rap.server.data.filter.Comparison.StringComparison;


/**
 * @author XDEV Software
 *
 */
public interface Filter extends Serializable
{
	public default Composite and(final Filter other)
	{
		return new Composite.Implementation(Composite.Connector.AND,this,other);
	}


	public default Composite or(final Filter other)
	{
		return new Composite.Implementation(Composite.Connector.OR,this,other);
	}


	@SafeVarargs
	public static Composite And(final Filter... filters)
	{
		return new Composite.Implementation(Composite.Connector.AND,filters);
	}


	public static Composite And(final List<Filter> filters)
	{
		return new Composite.Implementation(Composite.Connector.AND,filters);
	}


	@SafeVarargs
	public static Composite Or(final Filter... filters)
	{
		return new Composite.Implementation(Composite.Connector.OR,filters);
	}


	public static Composite Or(final List<Filter> filters)
	{
		return new Composite.Implementation(Composite.Connector.OR,filters);
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
		return new Equals.Implementation(identifier,value);
	}
	
	
	public static Greater Greater(final Object identifier, final Comparable<?> value)
	{
		return new Greater.Implementation(identifier,value);
	}
	
	
	public static GreaterEquals GreaterEquals(final Object identifier, final Comparable<?> value)
	{
		return new GreaterEquals.Implementation(identifier,value);
	}
	
	
	public static Less Less(final Object identifier, final Comparable<?> value)
	{
		return new Less.Implementation(identifier,value);
	}
	
	
	public static LessEquals LessEquals(final Object identifier, final Comparable<?> value)
	{
		return new LessEquals.Implementation(identifier,value);
	}


	public static StringComparison StringComparison(final Object identifier, final String value,
			final boolean caseSensitive)
	{
		return new StringComparison.Implementation(identifier,value,caseSensitive);
	}


	public static StringComparison StringComparison(final Object identifier, final String value,
			final boolean caseSensitive, final List<Character> wildcards)
	{
		return new StringComparison.Implementation(identifier,value,caseSensitive,wildcards);
	}


	public static Between Between(final Object identifier, final Comparable<?> start,
			final Comparable<?> end)
	{
		return new Between.Implementation(identifier,start,end);
	}
}

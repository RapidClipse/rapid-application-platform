/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.data.filter;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;


/**
 * @author XDEV Software
 *
 */
public interface Comparison extends Filter
{
	public Object identifier();

	public Object value();

	public static abstract class Abstract implements Comparison
	{
		private final Object identifier;
		private final Object value;

		protected Abstract(final Object identifier, final Object value)
		{
			super();

			this.identifier = identifier;
			this.value      = value;
		}

		@Override
		public Object identifier()
		{
			return this.identifier;
		}

		@Override
		public Object value()
		{
			return this.value;
		}
	}

	public static Equals Equals(final Object identifier, final Object value)
	{
		return new Equals.Default(identifier, value);
	}

	public static interface Equals extends Comparison
	{
		public static class Default extends Abstract implements Equals
		{
			protected Default(final Object identifier, final Object value)
			{
				super(identifier, value);
			}
		}
	}

	public static interface SizeComparison extends Comparison
	{
		@Override
		public Comparable<?> value();

		public static class Abstract extends Comparison.Abstract implements SizeComparison
		{
			protected Abstract(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}

			@Override
			public Comparable<?> value()
			{
				return (Comparable<?>)super.value();
			}
		}
	}

	public static Greater Greater(final Object identifier, final Comparable<?> value)
	{
		return new Greater.Default(identifier, value);
	}

	public static interface Greater extends SizeComparison
	{
		public static class Default extends Abstract implements Greater
		{
			protected Default(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}
		}
	}

	public static GreaterEquals GreaterEquals(final Object identifier, final Comparable<?> value)
	{
		return new GreaterEquals.Default(identifier, value);
	}

	public static interface GreaterEquals extends SizeComparison
	{
		public static class Default extends Abstract implements GreaterEquals
		{
			protected Default(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}
		}
	}

	public static Less Less(final Object identifier, final Comparable<?> value)
	{
		return new Less.Default(identifier, value);
	}

	public static interface Less extends SizeComparison
	{
		public static class Default extends Abstract implements Less
		{
			protected Default(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}
		}
	}

	public static LessEquals LessEquals(final Object identifier, final Comparable<?> value)
	{
		return new LessEquals.Default(identifier, value);
	}

	public static interface LessEquals extends SizeComparison
	{
		public static class Default extends Abstract implements LessEquals
		{
			protected Default(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}
		}
	}

	public static StringComparison StringComparison(
		final Object identifier,
		final String value,
		final boolean caseSensitive)
	{
		return new StringComparison.Default(identifier, value, caseSensitive);
	}

	public static StringComparison StringComparison(
		final Object identifier,
		final String value,
		final boolean caseSensitive,
		final List<Character> wildcards)
	{
		return new StringComparison.Default(identifier, value, caseSensitive, wildcards);
	}
	
	public static interface StringComparison extends Comparison
	{
		public final static char DEFAULT_WILDCARD = '*';

		public final static List<Character> DEFAULT_WILDCARDS = Collections
			.unmodifiableList(Arrays.asList(DEFAULT_WILDCARD));

		@Override
		public String value();

		public boolean caseSensitive();

		public List<Character> wildcards();

		public static class Default extends Abstract implements StringComparison
		{
			private final boolean         caseSensitive;
			private final List<Character> wildcards;

			protected Default(
				final Object identifier,
				final String value,
				final boolean caseSensitive)
			{
				this(identifier, value, caseSensitive, DEFAULT_WILDCARDS);
			}

			protected Default(
				final Object identifier,
				final String value,
				final boolean caseSensitive,
				final List<Character> wildcards)
			{
				super(identifier, value);
				this.caseSensitive = caseSensitive;
				this.wildcards     = Collections.unmodifiableList(wildcards);
			}

			@Override
			public String value()
			{
				return (String)super.value();
			}

			@Override
			public boolean caseSensitive()
			{
				return this.caseSensitive;
			}

			@Override
			public List<Character> wildcards()
			{
				return this.wildcards;
			}
		}
	}
}

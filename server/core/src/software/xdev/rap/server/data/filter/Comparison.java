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


	public Comparable<?> value();



	public static abstract class Abstract implements Comparison
	{
		private final Object		identifier;
		private final Comparable<?>	value;


		public Abstract(final Object identifier, final Comparable<?> value)
		{
			super();

			this.identifier = identifier;
			this.value = value;
		}


		@Override
		public Object identifier()
		{
			return this.identifier;
		}


		@Override
		public Comparable<?> value()
		{
			return this.value;
		}
	}



	public static interface Equals extends Comparison
	{
		public static class Implementation extends Abstract implements Equals
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier,value);
			}
		}
	}



	public static interface Greater extends Comparison
	{
		public static class Implementation extends Abstract implements Greater
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier,value);
			}
		}
	}



	public static interface GreaterEquals extends Comparison
	{
		public static class Implementation extends Abstract implements GreaterEquals
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier,value);
			}
		}
	}



	public static interface Less extends Comparison
	{
		public static class Implementation extends Abstract implements Less
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier,value);
			}
		}
	}



	public static interface LessEquals extends Comparison
	{
		public static class Implementation extends Abstract implements LessEquals
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier,value);
			}
		}
	}



	public static interface StringComparison extends Comparison
	{
		public final static List<Character> DEFAULT_WILDCARDS = Collections
				.unmodifiableList(Arrays.asList('*','%'));


		@Override
		public String value();


		public boolean caseSensitive();


		public List<Character> wildcards();



		public static class Implementation extends Abstract implements StringComparison
		{
			private final boolean			caseSensitive;
			private final List<Character>	wildcards;


			public Implementation(final Object identifier, final String value,
					final boolean caseSensitive)
			{
				this(identifier,value,caseSensitive,DEFAULT_WILDCARDS);
			}


			public Implementation(final Object identifier, final String value,
					final boolean caseSensitive, final List<Character> wildcards)
			{
				super(identifier,value);
				this.caseSensitive = caseSensitive;
				this.wildcards = Collections.unmodifiableList(wildcards);
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

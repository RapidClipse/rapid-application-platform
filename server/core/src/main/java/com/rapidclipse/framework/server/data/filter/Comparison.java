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
		
		public Abstract(final Object identifier, final Object value)
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
	
	public static interface Equals extends Comparison
	{
		public static class Implementation extends Abstract implements Equals
		{
			public Implementation(final Object identifier, final Object value)
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
			public Abstract(final Object identifier, final Comparable<?> value)
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
	
	public static interface Greater extends SizeComparison
	{
		public static class Implementation extends Abstract implements Greater
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}
		}
	}
	
	public static interface GreaterEquals extends SizeComparison
	{
		public static class Implementation extends Abstract implements GreaterEquals
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}
		}
	}
	
	public static interface Less extends SizeComparison
	{
		public static class Implementation extends Abstract implements Less
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}
		}
	}
	
	public static interface LessEquals extends SizeComparison
	{
		public static class Implementation extends Abstract implements LessEquals
		{
			public Implementation(final Object identifier, final Comparable<?> value)
			{
				super(identifier, value);
			}
		}
	}
	
	public static interface StringComparison extends Comparison
	{
		public final static char            DEFAULT_WILDCARD  = '*';
		
		public final static List<Character> DEFAULT_WILDCARDS = Collections
			.unmodifiableList(Arrays.asList(DEFAULT_WILDCARD));
		
		@Override
		public String value();
		
		public boolean caseSensitive();
		
		public List<Character> wildcards();
		
		public static class Implementation extends Abstract implements StringComparison
		{
			private final boolean         caseSensitive;
			private final List<Character> wildcards;
			
			public Implementation(
				final Object identifier,
				final String value,
				final boolean caseSensitive)
			{
				this(identifier, value, caseSensitive, DEFAULT_WILDCARDS);
			}
			
			public Implementation(
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
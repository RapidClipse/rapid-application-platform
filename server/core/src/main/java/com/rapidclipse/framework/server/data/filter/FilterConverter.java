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
import java.util.function.BinaryOperator;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.data.filter.Comparison.Equals;
import com.rapidclipse.framework.server.data.filter.Comparison.Greater;
import com.rapidclipse.framework.server.data.filter.Comparison.GreaterEquals;
import com.rapidclipse.framework.server.data.filter.Comparison.Less;
import com.rapidclipse.framework.server.data.filter.Comparison.LessEquals;
import com.rapidclipse.framework.server.data.filter.Comparison.SizeComparison;
import com.rapidclipse.framework.server.data.filter.Comparison.StringComparison;
import com.rapidclipse.framework.server.data.filter.Composite.Connector;
import com.vaadin.flow.data.binder.BeanPropertySet;
import com.vaadin.flow.function.SerializableFunction;
import com.vaadin.flow.function.SerializablePredicate;
import com.vaadin.flow.function.ValueProvider;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface FilterConverter<R> extends SerializableFunction<Filter, R>
{
	@Override
	public R apply(Filter t);
	
	public static <T> SerializablePredicate<T> toSerializablePredicate(final Filter filter)
	{
		return new SerializablePredicateFilterConverter<T>().apply(filter);
	}
	
	public static <T> FilterConverter<SerializablePredicate<T>> SerializablePredicate()
	{
		return new SerializablePredicateFilterConverter<>();
	}
	
	public static class SerializablePredicateFilterConverter<T>
		implements FilterConverter<SerializablePredicate<T>>
	{
		public SerializablePredicateFilterConverter()
		{
			super();
		}
		
		@Override
		public SerializablePredicate<T> apply(final Filter filter)
		{
			if(filter == null)
			{
				return null;
			}
			
			if(filter instanceof Composite)
			{
				final Composite                                composite = (Composite)filter;
				final BinaryOperator<SerializablePredicate<T>> operator  = composite
					.connector() == Connector.AND ? (p1, p2) -> p1.and(p2)
						: (p1, p2) -> p1.or(p2);
				return composite.filters().stream().map(this::apply)
					.collect(Collectors.reducing(operator)).get();
			}
			
			if(filter instanceof SizeComparison)
			{
				final SizeComparison comparison = (SizeComparison)filter;
				final Object         identifier = comparison.identifier();
				final Comparable<?>  value      = comparison.value();
				if(filter instanceof Greater)
				{
					return new SizePredicate<>(identifier, value, result -> result > 0);
				}
				if(filter instanceof GreaterEquals)
				{
					return new SizePredicate<>(identifier, value, result -> result >= 0);
				}
				if(filter instanceof Less)
				{
					return new SizePredicate<>(identifier, value, result -> result < 0);
				}
				if(filter instanceof LessEquals)
				{
					return new SizePredicate<>(identifier, value, result -> result <= 0);
				}
			}
			
			if(filter instanceof Comparison)
			{
				final Comparison comparison = (Comparison)filter;
				final Object     identifier = comparison.identifier();
				final Object     value      = comparison.value();
				
				if(filter instanceof Equals)
				{
					return new EqualsPredicate<>(identifier, value);
				}
				
				if(filter instanceof StringComparison)
				{
					final StringComparison stringComparison = (StringComparison)filter;
					final String           term             = stringComparison.value();
					final List<Character>  wildcards        = stringComparison.wildcards();
					final boolean          caseSensitive    = stringComparison.caseSensitive();
					return new StringMatchPredicate<>(identifier, term, wildcards, caseSensitive);
				}
			}
			
			if(filter instanceof Between)
			{
				final Between       between    = (Between)filter;
				final Object        identifier = between.identifier();
				final Comparable<?> start      = between.start();
				final Comparable<?> end        = between.end();
				return new BetweenPredicate<>(identifier, start, end);
			}
			
			if(filter instanceof IsNull)
			{
				final IsNull isNull     = (IsNull)filter;
				final Object identifier = isNull.identifier();
				return new EqualsPredicate<>(identifier, null);
			}
			
			if(filter instanceof Not)
			{
				final Not not = (Not)filter;
				return apply(not.filter()).negate();
			}
			
			throw new IllegalArgumentException(filter.toString());
		}
		
		@SuppressWarnings("rawtypes")
		protected static abstract class AbstractPredicate<T> implements SerializablePredicate<T>
		{
			private final Object  identifier;
			private ValueProvider getter;
			
			public AbstractPredicate(final Object identifier)
			{
				super();
				
				this.identifier = identifier;
			}
			
			protected ValueProvider getGetter(final T bean)
			{
				if(this.getter == null)
				{
					this.getter = BeanPropertySet.get(bean.getClass())
						.getProperty(this.identifier.toString()).get().getGetter();
				}
				
				return this.getter;
			}
			
			@SuppressWarnings("unchecked")
			protected Object getValue(final T bean)
			{
				if(bean == null)
				{
					return null;
				}
				
				try
				{
					return getGetter(bean).apply(bean);
				}
				catch(final NullPointerException e)
				{
					/*
					 * XWS-1767
					 * BeanPropertySet#NestedBeanPropertyDefinition doesn't handle null values in chained calls
					 */
					return null;
				}
			}
		}
		
		@SuppressWarnings("rawtypes")
		public static class SizePredicate<T> extends AbstractPredicate<T>
		{
			@FunctionalInterface
			public static interface ResultPredicate extends Serializable
			{
				public boolean test(int value);
			}
			
			private final Comparable      value;
			private final ResultPredicate predicate;
			
			public SizePredicate(
				final Object identifier,
				final Comparable value,
				final ResultPredicate predicate)
			{
				super(identifier);
				
				this.value     = value;
				this.predicate = predicate;
			}
			
			@Override
			public boolean test(final T object)
			{
				return this.predicate.test(compareTo(object));
			}
			
			@SuppressWarnings("unchecked")
			protected int compareTo(final T bean)
			{
				final Object other = getValue(bean);
				
				if(this.value == null)
				{
					return other == null ? 0 : -1;
				}
				
				if(other == null)
				{
					return 1;
				}
				
				if(other.getClass().isInstance(this.value))
				{
					return -this.value.compareTo(other);
				}
				
				if(this.value.getClass().isInstance(other))
				{
					return ((Comparable)other).compareTo(this.value);
				}
				
				throw new IllegalArgumentException(
					"Could not compare the arguments: " + this.value + ", " + other);
			}
		}
		
		public static class EqualsPredicate<T> extends AbstractPredicate<T>
		{
			private final Object value;
			
			public EqualsPredicate(final Object identifier, final Object value)
			{
				super(identifier);
				
				this.value = value;
			}
			
			@SuppressWarnings({"unchecked", "rawtypes"})
			@Override
			public boolean test(final T bean)
			{
				final Object other = getValue(bean);
				
				if(this.value == other)
				{
					return true;
				}
				
				if(this.value == null || other == null)
				{
					return other == this.value;
				}
				
				if(this.value instanceof Comparable
					&& other.getClass().isAssignableFrom(this.value.getClass()))
				{
					return ((Comparable)this.value).compareTo(other) == 0;
				}
				
				return this.value.equals(other);
			}
		}
		
		public static class StringMatchPredicate<T> extends AbstractPredicate<T>
		{
			private final Pattern pattern;
			
			public StringMatchPredicate(
				final Object identifier,
				final String term,
				final List<Character> wildcards,
				final boolean caseSensitive)
			{
				super(identifier);
				
				int flags = Pattern.DOTALL;
				if(!caseSensitive)
				{
					flags |= Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE;
				}
				this.pattern = Pattern.compile(wildcardToRegex(term, wildcards), flags);
			}
			
			protected String wildcardToRegex(final String term, final List<Character> wildcards)
			{
				final StringBuilder sb = new StringBuilder((int)(term.length() * 1.5));
				
				for(final char ch : term.toCharArray())
				{
					if(wildcards.contains(ch))
					{
						sb.append(".*");
					}
					else
					{
						switch(ch)
						{
							case '.':
							case '$':
							case '^':
							case '|':
							case '+':
							case '\\':
							case '(':
							case ')':
							case '{':
							case '}':
							case '[':
							case ']':
								sb.append('\\');
								// escape and fall through
								
							default:
								sb.append(ch);
						}
					}
				}
				
				return sb.toString();
			}
			
			@Override
			public boolean test(final T bean)
			{
				final Object value = String.valueOf(getValue(bean));
				return this.pattern.matcher(String.valueOf(value)).matches();
			}
		}
		
		@SuppressWarnings("rawtypes")
		public static class BetweenPredicate<T> extends AbstractPredicate<T>
		{
			private final Comparable start;
			private final Comparable end;
			
			public BetweenPredicate(
				final Object identifier,
				final Comparable start,
				final Comparable end)
			{
				super(identifier);
				
				this.start = start;
				this.end   = end;
			}
			
			@SuppressWarnings("unchecked")
			@Override
			public boolean test(final T bean)
			{
				final Object value = getValue(bean);
				
				if(value instanceof Comparable)
				{
					final Comparable comparable = (Comparable)value;
					return (this.start == null || comparable.compareTo(this.start) >= 0)
						&& (this.end == null || comparable.compareTo(this.end) <= 0);
				}
				
				if(value == null)
				{
					return this.start == null && this.end == null;
				}
				
				return false;
			}
		}
	}
}

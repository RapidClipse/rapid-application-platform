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

package com.rapidclipse.framework.server.data.converter;

import static java.util.Objects.requireNonNull;

import java.text.DateFormat;
import java.text.ParsePosition;
import java.util.Date;
import java.util.Locale;
import java.util.function.Function;

import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 *
 */
public interface StringToDateConverter<MODEL extends Date> extends Converter<String, MODEL>
{
	@SuppressWarnings("unchecked")
	public static <MODEL extends Date> StringToDateConverter<MODEL> New(
		final Class<MODEL> clazz,
		final Function<Locale, DateFormat> dateFormatProvider)
	{
		if(Date.class.equals(clazz))
		{
			return (StringToDateConverter<MODEL>)UtilDate(dateFormatProvider);
		}
		if(java.sql.Date.class.equals(clazz))
		{
			return (StringToDateConverter<MODEL>)SqlDate(dateFormatProvider);
		}
		if(java.sql.Time.class.equals(clazz))
		{
			return (StringToDateConverter<MODEL>)SqlTime(dateFormatProvider);
		}
		if(java.sql.Timestamp.class.equals(clazz))
		{
			return (StringToDateConverter<MODEL>)SqlTimestamp(dateFormatProvider);
		}

		throw new IllegalArgumentException("Unsupported date type: " + clazz);
	}
	
	public static StringToDateConverter<Date> UtilDate(
		final Function<Locale, DateFormat> dateFormatProvider)
	{
		return new Implementation<>(dateFormatProvider, date -> date);
	}
	
	public static StringToDateConverter<java.sql.Date> SqlDate(
		final Function<Locale, DateFormat> dateFormatProvider)
	{
		return new Implementation<>(dateFormatProvider, date -> new java.sql.Date(date.getTime()));
	}
	
	public static StringToDateConverter<java.sql.Time> SqlTime(
		final Function<Locale, DateFormat> dateFormatProvider)
	{
		return new Implementation<>(dateFormatProvider, date -> new java.sql.Time(date.getTime()));
	}
	
	public static StringToDateConverter<java.sql.Timestamp> SqlTimestamp(
		final Function<Locale, DateFormat> dateFormatProvider)
	{
		return new Implementation<>(dateFormatProvider,
			date -> new java.sql.Timestamp(date.getTime()));
	}
	
	public static class Implementation<MODEL extends Date> implements StringToDateConverter<MODEL>
	{
		private final Function<Locale, DateFormat> dateFormatProvider;
		private final Function<Date, MODEL>        dateConverter;
		
		public Implementation(
			final Function<Locale, DateFormat> dateFormatProvider,
			final Function<Date, MODEL> dateConverter)
		{
			super();

			this.dateFormatProvider = requireNonNull(dateFormatProvider);
			this.dateConverter      = requireNonNull(dateConverter);
		}
		
		@Override
		public Result<MODEL> convertToModel(String value, final ValueContext context)
		{
			if(value == null)
			{
				return Result.ok(null);
			}

			value = value.trim();

			final ParsePosition parsePosition = new ParsePosition(0);
			final Date          parsedValue   = this.dateFormatProvider.apply(context.getLocale().orElse(null))
				.parse(value, parsePosition);
			if(parsePosition.getIndex() != value.length())
			{
				return Result.error("Could not convert '" + value + "'");
			}

			return Result.ok(dateConverter.apply(parsedValue));
		}
		
		@Override
		public String convertToPresentation(final MODEL value, final ValueContext context)
		{
			if(value == null)
			{
				return null;
			}

			final Locale locale = context.getLocale().orElse(null);
			return this.dateFormatProvider.apply(locale).format(value);
		}
	}
}

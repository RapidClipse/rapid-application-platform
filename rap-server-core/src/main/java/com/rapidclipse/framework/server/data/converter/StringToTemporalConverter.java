/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.converter;

import static java.util.Objects.requireNonNull;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.Temporal;
import java.util.Locale;
import java.util.function.BiFunction;
import java.util.function.Function;

import com.vaadin.flow.data.binder.ErrorMessageProvider;
import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 *
 */
public interface StringToTemporalConverter<MODEL extends Temporal> extends Converter<String, MODEL>
{
	@SuppressWarnings("unchecked")
	public static <MODEL extends Temporal> StringToTemporalConverter<MODEL> New(
		final Class<MODEL> clazz,
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		if(LocalDate.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)LocalDate(formatterProvider, errorMessageProvider);
		}
		if(LocalTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)LocalTime(formatterProvider, errorMessageProvider);
		}
		if(LocalDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)LocalDateTime(formatterProvider, errorMessageProvider);
		}
		if(OffsetTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)OffsetTime(formatterProvider, errorMessageProvider);
		}
		if(OffsetDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)OffsetDateTime(formatterProvider, errorMessageProvider);
		}
		if(ZonedDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)ZonedDateTime(formatterProvider, errorMessageProvider);
		}
		if(Year.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)Year(formatterProvider, errorMessageProvider);
		}
		if(YearMonth.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)YearMonth(formatterProvider, errorMessageProvider);
		}

		throw new IllegalArgumentException("Unsupported temporal type: " + clazz);
	}

	public static StringToTemporalConverter<LocalDate> LocalDate(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		return new Default<>(formatterProvider, LocalDate::parse, errorMessageProvider);
	}

	public static StringToTemporalConverter<LocalTime> LocalTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		return new Default<>(formatterProvider, LocalTime::parse, errorMessageProvider);
	}

	public static StringToTemporalConverter<LocalDateTime> LocalDateTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		return new Default<>(formatterProvider, LocalDateTime::parse, errorMessageProvider);
	}

	public static StringToTemporalConverter<OffsetTime> OffsetTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		return new Default<>(formatterProvider, OffsetTime::parse, errorMessageProvider);
	}

	public static StringToTemporalConverter<OffsetDateTime> OffsetDateTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		return new Default<>(formatterProvider, OffsetDateTime::parse, errorMessageProvider);
	}

	public static StringToTemporalConverter<ZonedDateTime> ZonedDateTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		return new Default<>(formatterProvider, ZonedDateTime::parse, errorMessageProvider);
	}

	public static StringToTemporalConverter<Year> Year(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		return new Default<>(formatterProvider, Year::parse, errorMessageProvider);
	}

	public static StringToTemporalConverter<YearMonth> YearMonth(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final ErrorMessageProvider errorMessageProvider)
	{
		return new Default<>(formatterProvider, YearMonth::parse, errorMessageProvider);
	}

	public static <MODEL extends Temporal> StringToTemporalConverter<MODEL> New(
		final Class<MODEL> clazz,
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return New(clazz, formatterProvider, ctx -> errorMessage);
	}

	public static StringToTemporalConverter<LocalDate> LocalDate(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return LocalDate(formatterProvider, ctx -> errorMessage);
	}

	public static StringToTemporalConverter<LocalTime> LocalTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return LocalTime(formatterProvider, ctx -> errorMessage);
	}

	public static StringToTemporalConverter<LocalDateTime> LocalDateTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return LocalDateTime(formatterProvider, ctx -> errorMessage);
	}

	public static StringToTemporalConverter<OffsetTime> OffsetTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return OffsetTime(formatterProvider, ctx -> errorMessage);
	}

	public static StringToTemporalConverter<OffsetDateTime> OffsetDateTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return OffsetDateTime(formatterProvider, ctx -> errorMessage);
	}

	public static StringToTemporalConverter<ZonedDateTime> ZonedDateTime(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return ZonedDateTime(formatterProvider, ctx -> errorMessage);
	}

	public static StringToTemporalConverter<Year> Year(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return Year(formatterProvider, ctx -> errorMessage);
	}

	public static StringToTemporalConverter<YearMonth> YearMonth(
		final Function<Locale, DateTimeFormatter> formatterProvider,
		final String errorMessage)
	{
		return YearMonth(formatterProvider, ctx -> errorMessage);
	}

	public static class Default<MODEL extends Temporal>
		implements StringToTemporalConverter<MODEL>
	{
		private final Function<Locale, DateTimeFormatter>          formatterProvider;
		private final BiFunction<String, DateTimeFormatter, MODEL> temporalParser;
		private final ErrorMessageProvider                         errorMessageProvider;

		public Default(
			final Function<Locale, DateTimeFormatter> formatterProvider,
			final BiFunction<String, DateTimeFormatter, MODEL> temporalParser,
			final ErrorMessageProvider errorMessageProvider)
		{
			super();

			this.formatterProvider    = requireNonNull(formatterProvider);
			this.temporalParser       = requireNonNull(temporalParser);
			this.errorMessageProvider = requireNonNull(errorMessageProvider);
		}

		@Override
		public Result<MODEL> convertToModel(String value, final ValueContext context)
		{
			if(value == null)
			{
				return Result.ok(null);
			}

			value = value.trim();

			try
			{
				final Locale            locale    = context.getLocale().orElse(null);
				final DateTimeFormatter formatter = this.formatterProvider.apply(locale);
				return Result.ok(this.temporalParser.apply(value, formatter));
			}
			catch(final DateTimeParseException e)
			{
				return Result.error(this.errorMessageProvider.apply(context));
			}
		}

		@Override
		public String convertToPresentation(final MODEL value, final ValueContext context)
		{
			if(value == null)
			{
				return null;
			}

			return this.formatterProvider.apply(context.getLocale().orElse(null)).format(value);
		}
	}
}

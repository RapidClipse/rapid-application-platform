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
			final Class<MODEL> clazz, final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		if(LocalDate.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)LocalDate(formatterProvider);
		}
		if(LocalTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)LocalTime(formatterProvider);
		}
		if(LocalDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)LocalDateTime(formatterProvider);
		}
		if(OffsetTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)OffsetTime(formatterProvider);
		}
		if(OffsetDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)OffsetDateTime(formatterProvider);
		}
		if(ZonedDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)ZonedDateTime(formatterProvider);
		}
		if(Year.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)Year(formatterProvider);
		}
		if(YearMonth.class.equals(clazz))
		{
			return (StringToTemporalConverter<MODEL>)YearMonth(formatterProvider);
		}

		throw new IllegalArgumentException("Unsupported temporal type: " + clazz);
	}


	public static StringToTemporalConverter<LocalDate> LocalDate(
			final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		return new Implementation<>(formatterProvider,LocalDate::parse);
	}


	public static StringToTemporalConverter<LocalTime> LocalTime(
			final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		return new Implementation<>(formatterProvider,LocalTime::parse);
	}


	public static StringToTemporalConverter<LocalDateTime> LocalDateTime(
			final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		return new Implementation<>(formatterProvider,LocalDateTime::parse);
	}


	public static StringToTemporalConverter<OffsetTime> OffsetTime(
			final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		return new Implementation<>(formatterProvider,OffsetTime::parse);
	}


	public static StringToTemporalConverter<OffsetDateTime> OffsetDateTime(
			final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		return new Implementation<>(formatterProvider,OffsetDateTime::parse);
	}


	public static StringToTemporalConverter<ZonedDateTime> ZonedDateTime(
			final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		return new Implementation<>(formatterProvider,ZonedDateTime::parse);
	}


	public static StringToTemporalConverter<Year> Year(
			final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		return new Implementation<>(formatterProvider,Year::parse);
	}


	public static StringToTemporalConverter<YearMonth> YearMonth(
			final Function<Locale, DateTimeFormatter> formatterProvider)
	{
		return new Implementation<>(formatterProvider,YearMonth::parse);
	}



	public static class Implementation<MODEL extends Temporal>
			implements StringToTemporalConverter<MODEL>
	{
		private final Function<Locale, DateTimeFormatter>			formatterProvider;
		private final BiFunction<String, DateTimeFormatter, MODEL>	temporalParser;


		public Implementation(final Function<Locale, DateTimeFormatter> formatterProvider,
				final BiFunction<String, DateTimeFormatter, MODEL> temporalParser)
		{
			super();

			this.formatterProvider = requireNonNull(formatterProvider);
			this.temporalParser = requireNonNull(temporalParser);
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
				final Locale locale = context.getLocale().orElse(null);
				final DateTimeFormatter formatter = this.formatterProvider.apply(locale);
				return Result.ok(this.temporalParser.apply(value,formatter));
			}
			catch(final DateTimeParseException e)
			{
				return Result.error(e.getLocalizedMessage());
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

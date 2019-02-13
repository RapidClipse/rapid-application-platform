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
import java.time.format.DateTimeParseException;
import java.time.temporal.Temporal;
import java.util.function.Function;

import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 *
 */
public interface LocalDateToTemporalConverter<MODEL extends Temporal>
		extends Converter<LocalDate, MODEL>
{
	@SuppressWarnings("unchecked")
	public static <MODEL extends Temporal> LocalDateToTemporalConverter<MODEL> New(
			final Class<MODEL> clazz)
	{
		if(LocalDate.class.equals(clazz))
		{
			return (LocalDateToTemporalConverter<MODEL>)LocalDate();
		}
		if(LocalTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverter<MODEL>)LocalTime();
		}
		if(LocalDateTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverter<MODEL>)LocalDateTime();
		}
		if(OffsetTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverter<MODEL>)OffsetTime();
		}
		if(OffsetDateTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverter<MODEL>)OffsetDateTime();
		}
		if(ZonedDateTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverter<MODEL>)ZonedDateTime();
		}
		if(Year.class.equals(clazz))
		{
			return (LocalDateToTemporalConverter<MODEL>)Year();
		}
		if(YearMonth.class.equals(clazz))
		{
			return (LocalDateToTemporalConverter<MODEL>)YearMonth();
		}

		throw new IllegalArgumentException("Unsupported temporal type: " + clazz);
	}


	public static LocalDateToTemporalConverter<LocalDate> LocalDate()
	{
		return new Implementation<>(date -> date);
	}


	public static LocalDateToTemporalConverter<LocalTime> LocalTime()
	{
		return new Implementation<>(LocalTime::from);
	}


	public static LocalDateToTemporalConverter<LocalDateTime> LocalDateTime()
	{
		return new Implementation<>(LocalDateTime::from);
	}


	public static LocalDateToTemporalConverter<OffsetTime> OffsetTime()
	{
		return new Implementation<>(OffsetTime::from);
	}


	public static LocalDateToTemporalConverter<OffsetDateTime> OffsetDateTime()
	{
		return new Implementation<>(OffsetDateTime::from);
	}


	public static LocalDateToTemporalConverter<ZonedDateTime> ZonedDateTime()
	{
		return new Implementation<>(ZonedDateTime::from);
	}


	public static LocalDateToTemporalConverter<Year> Year()
	{
		return new Implementation<>(Year::from);
	}


	public static LocalDateToTemporalConverter<YearMonth> YearMonth()
	{
		return new Implementation<>(YearMonth::from);
	}



	public static class Implementation<MODEL extends Temporal>
			implements LocalDateToTemporalConverter<MODEL>
	{
		private final Function<LocalDate, MODEL> temporalConverter;


		public Implementation(final Function<LocalDate, MODEL> temporalConverter)
		{
			super();

			this.temporalConverter = requireNonNull(temporalConverter);
		}


		@Override
		public Result<MODEL> convertToModel(final LocalDate value, final ValueContext context)
		{
			if(value == null)
			{
				return Result.ok(null);
			}

			try
			{
				return Result.ok(this.temporalConverter.apply(value));
			}
			catch(final DateTimeParseException e)
			{
				return Result.error(e.getLocalizedMessage());
			}
		}


		@Override
		public LocalDate convertToPresentation(final MODEL value, final ValueContext context)
		{
			if(value == null)
			{
				return null;
			}

			return LocalDate.from(value);
		}
	}
}

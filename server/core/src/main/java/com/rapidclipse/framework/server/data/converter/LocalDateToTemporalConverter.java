/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.data.converter;

import static java.util.Objects.requireNonNull;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZoneId;
import java.time.ZonedDateTime;
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
		return new Default<>(date -> date);
	}

	public static LocalDateToTemporalConverter<LocalTime> LocalTime()
	{
		return new Default<>(date -> LocalTime.of(0, 0));
	}

	public static LocalDateToTemporalConverter<LocalDateTime> LocalDateTime()
	{
		return new Default<>(date -> date.atTime(0, 0));
	}

	public static LocalDateToTemporalConverter<OffsetTime> OffsetTime()
	{
		return new Default<>(
			date -> OffsetTime.of(LocalTime.of(0, 0), OffsetDateTime.now().getOffset()));
	}

	public static LocalDateToTemporalConverter<OffsetDateTime> OffsetDateTime()
	{
		return new Default<>(
			date -> OffsetDateTime.of(date.atTime(0, 0), OffsetDateTime.now().getOffset()));
	}

	public static LocalDateToTemporalConverter<ZonedDateTime> ZonedDateTime()
	{
		return new Default<>(date -> ZonedDateTime.of(date.atTime(0, 0), ZoneId.systemDefault()));
	}

	public static LocalDateToTemporalConverter<Year> Year()
	{
		return new Default<>(Year::from);
	}

	public static LocalDateToTemporalConverter<YearMonth> YearMonth()
	{
		return new Default<>(YearMonth::from);
	}

	public static class Default<MODEL extends Temporal>
		implements LocalDateToTemporalConverter<MODEL>
	{
		private final Function<LocalDate, MODEL> temporalConverter;

		public Default(final Function<LocalDate, MODEL> temporalConverter)
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

			return Result.ok(this.temporalConverter.apply(value));
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

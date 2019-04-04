/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.data.converter;

import static java.util.Objects.requireNonNull;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.function.Function;

import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 *
 */
public interface LocalDateToDateConverter<MODEL extends Date> extends Converter<LocalDate, MODEL>
{
	public static <MODEL extends Date> LocalDateToDateConverter<MODEL> New(final Class<MODEL> clazz)
	{
		return New(clazz, ZoneId.systemDefault());
	}
	
	@SuppressWarnings("unchecked")
	public static <MODEL extends Date> LocalDateToDateConverter<MODEL> New(
		final Class<MODEL> clazz,
		final ZoneId zoneId)
	{
		if(Date.class.equals(clazz))
		{
			return (LocalDateToDateConverter<MODEL>)UtilDate(zoneId);
		}
		if(java.sql.Date.class.equals(clazz))
		{
			return (LocalDateToDateConverter<MODEL>)SqlDate(zoneId);
		}
		if(java.sql.Time.class.equals(clazz))
		{
			return (LocalDateToDateConverter<MODEL>)SqlTime(zoneId);
		}
		if(java.sql.Timestamp.class.equals(clazz))
		{
			return (LocalDateToDateConverter<MODEL>)SqlTimestamp(zoneId);
		}
		
		throw new IllegalArgumentException("Unsupported date type: " + clazz);
	}
	
	public static LocalDateToDateConverter<Date> UtilDate()
	{
		return UtilDate(ZoneId.systemDefault());
	}
	
	public static LocalDateToDateConverter<Date> UtilDate(final ZoneId zoneId)
	{
		return new Implementation<>(zoneId, date -> date);
	}
	
	public static LocalDateToDateConverter<java.sql.Date> SqlDate()
	{
		return SqlDate(ZoneId.systemDefault());
	}
	
	public static LocalDateToDateConverter<java.sql.Date> SqlDate(final ZoneId zoneId)
	{
		return new Implementation<>(zoneId, date -> new java.sql.Date(date.getTime()));
	}
	
	public static LocalDateToDateConverter<java.sql.Time> SqlTime()
	{
		return SqlTime(ZoneId.systemDefault());
	}
	
	public static LocalDateToDateConverter<java.sql.Time> SqlTime(final ZoneId zoneId)
	{
		return new Implementation<>(zoneId, date -> new java.sql.Time(date.getTime()));
	}
	
	public static LocalDateToDateConverter<java.sql.Timestamp> SqlTimestamp()
	{
		return SqlTimestamp(ZoneId.systemDefault());
	}
	
	public static LocalDateToDateConverter<java.sql.Timestamp> SqlTimestamp(final ZoneId zoneId)
	{
		return new Implementation<>(zoneId, date -> new java.sql.Timestamp(date.getTime()));
	}
	
	public static class Implementation<MODEL extends Date>
		implements LocalDateToDateConverter<MODEL>
	{
		private ZoneId                      zoneId;
		private final Function<Date, MODEL> dateConverter;
		
		public Implementation(final Function<Date, MODEL> dateConverter)
		{
			this(ZoneId.systemDefault(), dateConverter);
		}
		
		public Implementation(final ZoneId zoneId, final Function<Date, MODEL> dateConverter)
		{
			super();
			
			this.zoneId        = requireNonNull(zoneId);
			this.dateConverter = requireNonNull(dateConverter);
		}
		
		@Override
		public Result<MODEL> convertToModel(final LocalDate value, final ValueContext context)
		{
			if(value == null)
			{
				return Result.ok(null);
			}
			
			return Result
				.ok(dateConverter.apply(Date.from(value.atStartOfDay(zoneId).toInstant())));
		}
		
		@Override
		public LocalDate convertToPresentation(final MODEL value, final ValueContext context)
		{
			if(value == null)
			{
				return null;
			}
			
			return Instant.ofEpochMilli(value.getTime()).atZone(zoneId).toLocalDate();
		}
	}
}

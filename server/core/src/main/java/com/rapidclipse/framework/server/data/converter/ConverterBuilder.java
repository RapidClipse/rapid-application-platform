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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.Temporal;
import java.util.Date;
import java.util.Locale;

import com.rapidclipse.framework.server.data.format.DateFormatBuilder;
import com.rapidclipse.framework.server.data.format.NumberFormatBuilder;
import com.vaadin.flow.data.binder.ErrorMessageProvider;
import com.vaadin.flow.data.converter.Converter;
import com.vaadin.flow.data.converter.StringToBigDecimalConverter;
import com.vaadin.flow.data.converter.StringToBigIntegerConverter;
import com.vaadin.flow.data.converter.StringToBooleanConverter;
import com.vaadin.flow.data.converter.StringToDoubleConverter;
import com.vaadin.flow.data.converter.StringToFloatConverter;
import com.vaadin.flow.data.converter.StringToIntegerConverter;
import com.vaadin.flow.data.converter.StringToLongConverter;


/**
 * @author XDEV Software
 *
 */
public interface ConverterBuilder<PRESENTATION, MODEL>
{
	public final static String DEFAULT_ERROR_MESSAGE = "Conversion error: {0}";

	public Converter<PRESENTATION, MODEL> build();

	@SuppressWarnings("unchecked")
	public static <MODEL extends Number> StringToNumberConverterBuilder<MODEL> StringToNumber(
		final Class<MODEL> clazz)
	{
		if(Byte.class.equals(clazz) || byte.class.equals(clazz))
		{
			return (StringToNumberConverterBuilder<MODEL>)StringToByte();
		}
		if(Short.class.equals(clazz) || short.class.equals(clazz))
		{
			return (StringToNumberConverterBuilder<MODEL>)StringToShort();
		}
		if(Integer.class.equals(clazz) || int.class.equals(clazz))
		{
			return (StringToNumberConverterBuilder<MODEL>)StringToInteger();
		}
		if(Long.class.equals(clazz) || long.class.equals(clazz))
		{
			return (StringToNumberConverterBuilder<MODEL>)StringToLong();
		}
		if(BigInteger.class.equals(clazz))
		{
			return (StringToNumberConverterBuilder<MODEL>)StringToBigInteger();
		}
		if(Float.class.equals(clazz) || float.class.equals(clazz))
		{
			return (StringToNumberConverterBuilder<MODEL>)StringToFloat();
		}
		if(Double.class.equals(clazz) || double.class.equals(clazz))
		{
			return (StringToNumberConverterBuilder<MODEL>)StringToDouble();
		}
		if(BigDecimal.class.equals(clazz))
		{
			return (StringToNumberConverterBuilder<MODEL>)StringToBigDecimal();
		}

		throw new IllegalArgumentException("Unsupported number type: " + clazz);
	}

	public static StringToNumberConverterBuilder<Byte> StringToByte()
	{
		return new StringToNumberConverterBuilder.Default<>(Byte.class);
	}

	public static StringToNumberConverterBuilder<Short> StringToShort()
	{
		return new StringToNumberConverterBuilder.Default<>(Short.class);
	}

	public static StringToNumberConverterBuilder<Integer> StringToInteger()
	{
		return new StringToNumberConverterBuilder.Default<>(Integer.class);
	}

	public static StringToNumberConverterBuilder<Long> StringToLong()
	{
		return new StringToNumberConverterBuilder.Default<>(Long.class);
	}

	public static StringToNumberConverterBuilder<BigInteger> StringToBigInteger()
	{
		return new StringToNumberConverterBuilder.Default<>(Integer.class);
	}

	public static StringToNumberConverterBuilder<Float> StringToFloat()
	{
		return new StringToNumberConverterBuilder.Default<>(Float.class);
	}

	public static StringToNumberConverterBuilder<Double> StringToDouble()
	{
		return new StringToNumberConverterBuilder.Default<>(Double.class);
	}

	public static StringToNumberConverterBuilder<BigDecimal> StringToBigDecimal()
	{
		return new StringToNumberConverterBuilder.Default<>(BigDecimal.class);
	}

	public static StringToBooleanConverterBuilder StringToBoolean()
	{
		return new StringToBooleanConverterBuilder.Default();
	}
	
	public static <E extends Enum<E>> StringToEnumConverterBuilder<E> StringToEnum(final Class<E> enumType)
	{
		return new StringToEnumConverterBuilder.Default<>(enumType);
	}

	@SuppressWarnings("unchecked")
	public static <MODEL extends Date> StringToDateConverterBuilder<MODEL> StringToDate(
		final Class<MODEL> clazz)
	{
		if(Date.class.equals(clazz))
		{
			return (StringToDateConverterBuilder<MODEL>)StringToUtilDate();
		}
		if(java.sql.Date.class.equals(clazz))
		{
			return (StringToDateConverterBuilder<MODEL>)StringToSqlDate();
		}
		if(java.sql.Time.class.equals(clazz))
		{
			return (StringToDateConverterBuilder<MODEL>)StringToSqlTime();
		}
		if(java.sql.Timestamp.class.equals(clazz))
		{
			return (StringToDateConverterBuilder<MODEL>)StringToSqlTimestamp();
		}

		throw new IllegalArgumentException("Unsupported date type: " + clazz);
	}

	public static StringToDateConverterBuilder<Date> StringToUtilDate()
	{
		return new StringToDateConverterBuilder.Default<>(Date.class);
	}

	public static StringToDateConverterBuilder<java.sql.Date> StringToSqlDate()
	{
		return new StringToDateConverterBuilder.Default<>(java.sql.Date.class);
	}

	public static StringToDateConverterBuilder<java.sql.Time> StringToSqlTime()
	{
		return new StringToDateConverterBuilder.Default<>(java.sql.Time.class);
	}

	public static StringToDateConverterBuilder<java.sql.Timestamp> StringToSqlTimestamp()
	{
		return new StringToDateConverterBuilder.Default<>(java.sql.Timestamp.class);
	}

	@SuppressWarnings("unchecked")
	public static <MODEL extends Temporal> StringToTemporalConverterBuilder<MODEL> StringToTemporal(
		final Class<MODEL> clazz)
	{
		if(LocalDate.class.equals(clazz))
		{
			return (StringToTemporalConverterBuilder<MODEL>)StringToLocalDate();
		}
		if(LocalTime.class.equals(clazz))
		{
			return (StringToTemporalConverterBuilder<MODEL>)StringToLocalTime();
		}
		if(LocalDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverterBuilder<MODEL>)StringToLocalDateTime();
		}
		if(OffsetTime.class.equals(clazz))
		{
			return (StringToTemporalConverterBuilder<MODEL>)StringToOffsetTime();
		}
		if(OffsetDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverterBuilder<MODEL>)StringToOffsetDateTime();
		}
		if(ZonedDateTime.class.equals(clazz))
		{
			return (StringToTemporalConverterBuilder<MODEL>)StringToZonedDateTime();
		}
		if(Year.class.equals(clazz))
		{
			return (StringToTemporalConverterBuilder<MODEL>)StringToYear();
		}
		if(YearMonth.class.equals(clazz))
		{
			return (StringToTemporalConverterBuilder<MODEL>)StringToYearMonth();
		}

		throw new IllegalArgumentException("Unsupported temporal type: " + clazz);
	}

	public static StringToTemporalConverterBuilder<LocalDate> StringToLocalDate()
	{
		return new StringToTemporalConverterBuilder.Default<>(LocalDate.class);
	}

	public static StringToTemporalConverterBuilder<LocalTime> StringToLocalTime()
	{
		return new StringToTemporalConverterBuilder.Default<>(LocalTime.class);
	}

	public static StringToTemporalConverterBuilder<LocalDateTime> StringToLocalDateTime()
	{
		return new StringToTemporalConverterBuilder.Default<>(LocalDateTime.class);
	}

	public static StringToTemporalConverterBuilder<OffsetTime> StringToOffsetTime()
	{
		return new StringToTemporalConverterBuilder.Default<>(OffsetTime.class);
	}

	public static StringToTemporalConverterBuilder<OffsetDateTime> StringToOffsetDateTime()
	{
		return new StringToTemporalConverterBuilder.Default<>(OffsetDateTime.class);
	}

	public static StringToTemporalConverterBuilder<ZonedDateTime> StringToZonedDateTime()
	{
		return new StringToTemporalConverterBuilder.Default<>(ZonedDateTime.class);
	}

	public static StringToTemporalConverterBuilder<Year> StringToYear()
	{
		return new StringToTemporalConverterBuilder.Default<>(Year.class);
	}

	public static StringToTemporalConverterBuilder<YearMonth> StringToYearMonth()
	{
		return new StringToTemporalConverterBuilder.Default<>(YearMonth.class);
	}

	@SuppressWarnings("unchecked")
	public static <MODEL extends Temporal> LocalDateToTemporalConverterBuilder<MODEL> LocalDateToTemporal(
		final Class<MODEL> clazz)
	{
		if(LocalTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverterBuilder<MODEL>)LocalDateToLocalTime();
		}
		if(LocalDateTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverterBuilder<MODEL>)LocalDateToLocalDateTime();
		}
		if(OffsetTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverterBuilder<MODEL>)LocalDateToOffsetTime();
		}
		if(OffsetDateTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverterBuilder<MODEL>)LocalDateToOffsetDateTime();
		}
		if(ZonedDateTime.class.equals(clazz))
		{
			return (LocalDateToTemporalConverterBuilder<MODEL>)LocalDateToZonedDateTime();
		}
		if(Year.class.equals(clazz))
		{
			return (LocalDateToTemporalConverterBuilder<MODEL>)LocalDateToYear();
		}
		if(YearMonth.class.equals(clazz))
		{
			return (LocalDateToTemporalConverterBuilder<MODEL>)LocalDateToYearMonth();
		}

		throw new IllegalArgumentException("Unsupported temporal type: " + clazz);
	}

	public static LocalDateToTemporalConverterBuilder<LocalTime> LocalDateToLocalTime()
	{
		return new LocalDateToTemporalConverterBuilder.Default<>(LocalTime.class);
	}

	public static LocalDateToTemporalConverterBuilder<LocalDateTime> LocalDateToLocalDateTime()
	{
		return new LocalDateToTemporalConverterBuilder.Default<>(LocalDateTime.class);
	}

	public static LocalDateToTemporalConverterBuilder<OffsetTime> LocalDateToOffsetTime()
	{
		return new LocalDateToTemporalConverterBuilder.Default<>(OffsetTime.class);
	}

	public static LocalDateToTemporalConverterBuilder<OffsetDateTime> LocalDateToOffsetDateTime()
	{
		return new LocalDateToTemporalConverterBuilder.Default<>(OffsetDateTime.class);
	}

	public static LocalDateToTemporalConverterBuilder<ZonedDateTime> LocalDateToZonedDateTime()
	{
		return new LocalDateToTemporalConverterBuilder.Default<>(ZonedDateTime.class);
	}

	public static LocalDateToTemporalConverterBuilder<Year> LocalDateToYear()
	{
		return new LocalDateToTemporalConverterBuilder.Default<>(Year.class);
	}

	public static LocalDateToTemporalConverterBuilder<YearMonth> LocalDateToYearMonth()
	{
		return new LocalDateToTemporalConverterBuilder.Default<>(YearMonth.class);
	}

	@SuppressWarnings("unchecked")
	public static <MODEL extends Date> LocalDateToDateConverterBuilder<MODEL> LocalDateToDate(
		final Class<MODEL> clazz)
	{
		if(Date.class.equals(clazz))
		{
			return (LocalDateToDateConverterBuilder<MODEL>)LocalDateToUtilDate();
		}
		if(java.sql.Date.class.equals(clazz))
		{
			return (LocalDateToDateConverterBuilder<MODEL>)LocalDateToSqlDate();
		}
		if(java.sql.Time.class.equals(clazz))
		{
			return (LocalDateToDateConverterBuilder<MODEL>)LocalDateToSqlTime();
		}
		if(java.sql.Timestamp.class.equals(clazz))
		{
			return (LocalDateToDateConverterBuilder<MODEL>)LocalDateToSqlTimestamp();
		}

		throw new IllegalArgumentException("Unsupported date type: " + clazz);
	}

	public static LocalDateToDateConverterBuilder<Date> LocalDateToUtilDate()
	{
		return new LocalDateToDateConverterBuilder.Default<>(Date.class);
	}

	public static LocalDateToDateConverterBuilder<java.sql.Date> LocalDateToSqlDate()
	{
		return new LocalDateToDateConverterBuilder.Default<>(java.sql.Date.class);
	}

	public static LocalDateToDateConverterBuilder<java.sql.Time> LocalDateToSqlTime()
	{
		return new LocalDateToDateConverterBuilder.Default<>(java.sql.Time.class);
	}

	public static LocalDateToDateConverterBuilder<java.sql.Timestamp> LocalDateToSqlTimestamp()
	{
		return new LocalDateToDateConverterBuilder.Default<>(java.sql.Timestamp.class);
	}

	@SuppressWarnings("unchecked")
	public static <MODEL extends Number> DoubleToNumberConverterBuilder<MODEL> DoubleToNumber(
		final Class<MODEL> clazz)
	{
		if(Byte.class.equals(clazz) || byte.class.equals(clazz))
		{
			return (DoubleToNumberConverterBuilder<MODEL>)DoubleToByte();
		}
		if(Short.class.equals(clazz) || short.class.equals(clazz))
		{
			return (DoubleToNumberConverterBuilder<MODEL>)DoubleToShort();
		}
		if(Integer.class.equals(clazz) || int.class.equals(clazz))
		{
			return (DoubleToNumberConverterBuilder<MODEL>)DoubleToInteger();
		}
		if(Long.class.equals(clazz) || long.class.equals(clazz))
		{
			return (DoubleToNumberConverterBuilder<MODEL>)DoubleToLong();
		}
		if(Float.class.equals(clazz) || float.class.equals(clazz))
		{
			return (DoubleToNumberConverterBuilder<MODEL>)DoubleToFloat();
		}
		if(BigInteger.class.equals(clazz))
		{
			return (DoubleToNumberConverterBuilder<MODEL>)DoubleToBigInteger();
		}
		if(BigDecimal.class.equals(clazz))
		{
			return (DoubleToNumberConverterBuilder<MODEL>)DoubleToBigDecimal();
		}

		throw new IllegalArgumentException("Unsupported number type: " + clazz);
	}

	public static DoubleToNumberConverterBuilder<Byte> DoubleToByte()
	{
		return new DoubleToNumberConverterBuilder.Default<>(Byte.class);
	}

	public static DoubleToNumberConverterBuilder<Short> DoubleToShort()
	{
		return new DoubleToNumberConverterBuilder.Default<>(Short.class);
	}

	public static DoubleToNumberConverterBuilder<Integer> DoubleToInteger()
	{
		return new DoubleToNumberConverterBuilder.Default<>(Integer.class);
	}

	public static DoubleToNumberConverterBuilder<Long> DoubleToLong()
	{
		return new DoubleToNumberConverterBuilder.Default<>(Long.class);
	}

	public static DoubleToNumberConverterBuilder<Float> DoubleToFloat()
	{
		return new DoubleToNumberConverterBuilder.Default<>(Float.class);
	}

	public static DoubleToNumberConverterBuilder<BigInteger> DoubleToBigInteger()
	{
		return new DoubleToNumberConverterBuilder.Default<>(BigInteger.class);
	}

	public static DoubleToNumberConverterBuilder<BigDecimal> DoubleToBigDecimal()
	{
		return new DoubleToNumberConverterBuilder.Default<>(BigDecimal.class);
	}

	public interface StringToNumberConverterBuilder<MODEL extends Number>
		extends ConverterBuilder<String, MODEL>
	{
		public StringToNumberConverterBuilder<MODEL> errorMessage(final String errorMessage);

		public StringToNumberConverterBuilder<MODEL>
			errorMessageProvider(final ErrorMessageProvider errorMessageProvider);

		public StringToNumberConverterBuilder<MODEL>
			numberFormatBuilder(final NumberFormatBuilder<?> numberFormatBuilder);

		public StringToNumberConverterBuilder<MODEL> emptyValue(final MODEL emptyValue);

		public static class Default<MODEL extends Number> implements StringToNumberConverterBuilder<MODEL>
		{
			private final Class<? extends Number> targetType;

			private ErrorMessageProvider   errorMessageProvider = context -> DEFAULT_ERROR_MESSAGE;
			private NumberFormatBuilder<?> numberFormatBuilder;
			private MODEL                  emptyValue;

			protected Default(final Class<? extends Number> targetType)
			{
				this.targetType = targetType;
			}

			@Override
			public StringToNumberConverterBuilder<MODEL> errorMessage(final String errorMessage)
			{
				this.errorMessageProvider = context -> errorMessage;
				return this;
			}

			@Override
			public StringToNumberConverterBuilder<MODEL> errorMessageProvider(
				final ErrorMessageProvider errorMessageProvider)
			{
				this.errorMessageProvider = errorMessageProvider;
				return this;
			}

			@Override
			public StringToNumberConverterBuilder<MODEL> numberFormatBuilder(
				final NumberFormatBuilder<?> numberFormatBuilder)
			{
				this.numberFormatBuilder = numberFormatBuilder;
				return this;
			}

			@Override
			public StringToNumberConverterBuilder<MODEL> emptyValue(final MODEL emptyValue)
			{
				this.emptyValue = emptyValue;
				return this;
			}

			@SuppressWarnings("unchecked")
			@Override
			public Converter<String, MODEL> build()
			{
				if(Byte.class.equals(this.targetType))
				{
					return (Converter<String, MODEL>)new StringToByteConverter((Byte)this.emptyValue,
						this.errorMessageProvider)
					{
						@Override
						protected NumberFormat getFormat(final Locale locale)
						{
							return Default.this.getFormat(locale);
						}
					};
				}

				if(Short.class.equals(this.targetType))
				{
					return (Converter<String, MODEL>)new StringToShortConverter((Short)this.emptyValue,
						this.errorMessageProvider)
					{

						@Override
						protected NumberFormat getFormat(final Locale locale)
						{
							return Default.this.getFormat(locale);
						}
					};
				}

				if(Integer.class.equals(this.targetType))

				{
					return (Converter<String, MODEL>)new StringToIntegerConverter(
						(Integer)this.emptyValue, this.errorMessageProvider)
					{
						@Override
						protected NumberFormat getFormat(final Locale locale)
						{
							return Default.this.getFormat(locale);
						}
					};
				}

				if(Long.class.equals(this.targetType))
				{
					return (Converter<String, MODEL>)new StringToLongConverter((Long)this.emptyValue,
						this.errorMessageProvider)
					{
						@Override
						protected NumberFormat getFormat(final Locale locale)
						{
							return Default.this.getFormat(locale);
						}
					};
				}

				if(BigInteger.class.equals(this.targetType))
				{
					return (Converter<String, MODEL>)new StringToBigIntegerConverter(
						(BigInteger)this.emptyValue, this.errorMessageProvider)
					{
						@Override
						protected NumberFormat getFormat(final Locale locale)
						{
							final NumberFormat format = Default.this.getFormat(locale);
							if(format instanceof DecimalFormat)
							{
								((DecimalFormat)format).setParseBigDecimal(true);
							}
							return format;
						}
					};
				}

				if(Float.class.equals(this.targetType))
				{
					return (Converter<String, MODEL>)new StringToFloatConverter((Float)this.emptyValue,
						this.errorMessageProvider)
					{
						@Override
						protected NumberFormat getFormat(final Locale locale)
						{
							return Default.this.getFormat(locale);
						}
					};
				}

				if(Double.class.equals(this.targetType))
				{
					return (Converter<String, MODEL>)new StringToDoubleConverter(
						(Double)this.emptyValue, this.errorMessageProvider)
					{
						@Override
						protected NumberFormat getFormat(final Locale locale)
						{
							return Default.this.getFormat(locale);
						}
					};
				}

				if(BigDecimal.class.equals(this.targetType))
				{
					return (Converter<String, MODEL>)new StringToBigDecimalConverter(
						(BigDecimal)this.emptyValue, this.errorMessageProvider)
					{
						@Override
						protected NumberFormat getFormat(final Locale locale)
						{
							final NumberFormat format = Default.this.getFormat(locale);
							if(format instanceof DecimalFormat)
							{
								((DecimalFormat)format).setParseBigDecimal(true);
							}
							return format;
						}
					};
				}

				return null;
			}

			private NumberFormat getFormat(final Locale locale)
			{
				if(this.numberFormatBuilder == null)
				{
					this.numberFormatBuilder =
						this.targetType == Float.class || this.targetType == Double.class
							? NumberFormatBuilder.Decimal()
							: NumberFormatBuilder.Integer();
				}

				return this.numberFormatBuilder.locale(locale).build();
			}
		}
	}

	public interface StringToBooleanConverterBuilder extends ConverterBuilder<String, Boolean>
	{
		public StringToBooleanConverterBuilder errorMessage(final String errorMessage);

		public StringToBooleanConverterBuilder errorMessageProvider(
			final ErrorMessageProvider errorMessageProvider);

		public StringToBooleanConverterBuilder trueString(final String trueString);

		public StringToBooleanConverterBuilder falseString(final String falseString);

		public static class Default implements StringToBooleanConverterBuilder
		{
			private ErrorMessageProvider errorMessageProvider = context -> DEFAULT_ERROR_MESSAGE;

			private String trueString  = Boolean.TRUE.toString();
			private String falseString = Boolean.FALSE.toString();

			protected Default()
			{
			}

			@Override
			public StringToBooleanConverterBuilder errorMessage(final String errorMessage)
			{
				this.errorMessageProvider = context -> errorMessage;
				return this;
			}

			@Override
			public StringToBooleanConverterBuilder errorMessageProvider(
				final ErrorMessageProvider errorMessageProvider)
			{
				this.errorMessageProvider = errorMessageProvider;
				return this;
			}

			@Override
			public StringToBooleanConverterBuilder trueString(final String trueString)
			{
				this.trueString = trueString;
				return this;
			}

			@Override
			public StringToBooleanConverterBuilder falseString(final String falseString)
			{
				this.falseString = falseString;
				return this;
			}

			@Override
			public Converter<String, Boolean> build()
			{
				return new StringToBooleanConverter(this.trueString, this.falseString,
					this.errorMessageProvider);
			}
		}
	}

	public interface StringToEnumConverterBuilder<E extends Enum<E>> extends ConverterBuilder<String, E>
	{
		public StringToEnumConverterBuilder<E> errorMessage(final String errorMessage);

		public StringToEnumConverterBuilder<E> errorMessageProvider(
			final ErrorMessageProvider errorMessageProvider);

		public StringToEnumConverterBuilder<E> emptyValue(final E emptyValue);

		public static class Default<E extends Enum<E>> implements StringToEnumConverterBuilder<E>
		{
			private final Class<E>       enumType;
			private E                    emptyValue;
			private ErrorMessageProvider errorMessageProvider = context -> DEFAULT_ERROR_MESSAGE;

			protected Default(final Class<E> enumType)
			{
				this.enumType = enumType;
			}

			@Override
			public StringToEnumConverterBuilder<E> errorMessage(final String errorMessage)
			{
				this.errorMessageProvider = context -> errorMessage;
				return this;
			}

			@Override
			public StringToEnumConverterBuilder<E> errorMessageProvider(
				final ErrorMessageProvider errorMessageProvider)
			{
				this.errorMessageProvider = errorMessageProvider;
				return this;
			}

			@Override
			public StringToEnumConverterBuilder<E> emptyValue(final E emptyValue)
			{
				this.emptyValue = emptyValue;
				return this;
			}

			@Override
			public Converter<String, E> build()
			{
				return new StringToEnumConverter<>(this.enumType, this.emptyValue, this.errorMessageProvider);
			}
		}
	}

	public interface StringToDateConverterBuilder<MODEL extends Date>
		extends ConverterBuilder<String, MODEL>
	{
		public StringToDateConverterBuilder<MODEL> errorMessage(final String errorMessage);

		public StringToDateConverterBuilder<MODEL> errorMessageProvider(
			final ErrorMessageProvider errorMessageProvider);
		
		public StringToDateConverterBuilder<MODEL> dateFormatBuilder(final DateFormatBuilder<?> dateFormatBuilder);

		public static class Default<MODEL extends Date> implements StringToDateConverterBuilder<MODEL>
		{
			private final Class<MODEL>   targetType;
			private ErrorMessageProvider errorMessageProvider = context -> DEFAULT_ERROR_MESSAGE;
			private DateFormatBuilder<?> dateFormatBuilder;

			protected Default(final Class<MODEL> targetType)
			{
				super();

				this.targetType = targetType;
			}

			@Override
			public StringToDateConverterBuilder<MODEL> errorMessage(final String errorMessage)
			{
				this.errorMessageProvider = context -> errorMessage;
				return this;
			}

			@Override
			public StringToDateConverterBuilder<MODEL> errorMessageProvider(
				final ErrorMessageProvider errorMessageProvider)
			{
				this.errorMessageProvider = errorMessageProvider;
				return this;
			}

			@Override
			public StringToDateConverterBuilder<MODEL> dateFormatBuilder(final DateFormatBuilder<?> dateFormatBuilder)
			{
				this.dateFormatBuilder = dateFormatBuilder;
				return this;
			}

			@Override
			public Converter<String, MODEL> build()
			{
				return StringToDateConverter.New(this.targetType, this::getFormat, this.errorMessageProvider);
			}

			private DateFormat getFormat(final Locale locale)
			{
				if(this.dateFormatBuilder != null)
				{
					this.dateFormatBuilder = java.sql.Time.class.equals(this.targetType) ? DateFormatBuilder.Time()
						: DateFormatBuilder.DateTime();
				}

				return this.dateFormatBuilder.locale(locale).build();
			}
		}
	}

	public interface StringToTemporalConverterBuilder<MODEL extends Temporal>
		extends ConverterBuilder<String, MODEL>
	{
		public StringToTemporalConverterBuilder<MODEL> errorMessage(final String errorMessage);

		public StringToTemporalConverterBuilder<MODEL> errorMessageProvider(
			final ErrorMessageProvider errorMessageProvider);
		
		public StringToTemporalConverterBuilder<MODEL>
			dateTimeFormatter(final DateTimeFormatter dateTimeFormatter);

		public static class Default<MODEL extends Temporal> implements StringToTemporalConverterBuilder<MODEL>
		{
			private final Class<MODEL>   targetType;
			private ErrorMessageProvider errorMessageProvider = context -> DEFAULT_ERROR_MESSAGE;
			private DateTimeFormatter    dateTimeFormatter;

			protected Default(final Class<MODEL> targetType)
			{
				super();

				this.targetType = targetType;
			}

			@Override
			public StringToTemporalConverterBuilder<MODEL> errorMessage(final String errorMessage)
			{
				this.errorMessageProvider = context -> errorMessage;
				return this;
			}

			@Override
			public StringToTemporalConverterBuilder<MODEL> errorMessageProvider(
				final ErrorMessageProvider errorMessageProvider)
			{
				this.errorMessageProvider = errorMessageProvider;
				return this;
			}

			@Override
			public StringToTemporalConverterBuilder<MODEL>
				dateTimeFormatter(final DateTimeFormatter dateTimeFormatter)
			{
				this.dateTimeFormatter = dateTimeFormatter;
				return this;
			}

			@Override
			public Converter<String, MODEL> build()
			{
				return StringToTemporalConverter.New(this.targetType, this::getFormatter, this.errorMessageProvider);
			}

			private DateTimeFormatter getFormatter(final Locale locale)
			{
				if(this.dateTimeFormatter == null)
				{
					this.dateTimeFormatter = DateTimeFormatter.BASIC_ISO_DATE;
				}

				return locale != null ? this.dateTimeFormatter.withLocale(locale) : this.dateTimeFormatter;
			}
		}
	}

	public interface LocalDateToTemporalConverterBuilder<MODEL extends Temporal>
		extends ConverterBuilder<LocalDate, MODEL>
	{
		public static class Default<MODEL extends Temporal> implements LocalDateToTemporalConverterBuilder<MODEL>
		{
			private final Class<MODEL> targetType;

			protected Default(final Class<MODEL> targetType)
			{
				super();

				this.targetType = targetType;
			}

			@Override
			public Converter<LocalDate, MODEL> build()
			{
				return LocalDateToTemporalConverter.New(this.targetType);
			}
		}
	}

	public interface LocalDateToDateConverterBuilder<MODEL extends Date>
		extends ConverterBuilder<LocalDate, MODEL>
	{
		public LocalDateToDateConverterBuilder<MODEL> zoneId(final ZoneId zoneId);

		public LocalDateToDateConverterBuilder<MODEL> systemDefaultZoneId();

		public static class Default<MODEL extends Date> implements LocalDateToDateConverterBuilder<MODEL>
		{
			private final Class<MODEL> targetType;
			private ZoneId             zoneId;

			protected Default(final Class<MODEL> targetType)
			{
				super();

				this.targetType = targetType;
			}

			@Override
			public LocalDateToDateConverterBuilder<MODEL> zoneId(final ZoneId zoneId)
			{
				this.zoneId = zoneId;
				return this;
			}

			@Override
			public LocalDateToDateConverterBuilder<MODEL> systemDefaultZoneId()
			{
				this.zoneId = null;
				return this;
			}

			@Override
			public Converter<LocalDate, MODEL> build()
			{
				final ZoneId zoneId = this.zoneId != null ? this.zoneId : ZoneId.systemDefault();
				return LocalDateToDateConverter.New(this.targetType, zoneId);
			}
		}
	}

	public interface DoubleToNumberConverterBuilder<MODEL extends Number>
		extends ConverterBuilder<Double, MODEL>
	{
		public static class Default<MODEL extends Number> implements DoubleToNumberConverterBuilder<MODEL>
		{
			private final Class<MODEL> targetType;

			protected Default(final Class<MODEL> targetType)
			{
				super();

				this.targetType = targetType;
			}

			@Override
			public Converter<Double, MODEL> build()
			{
				return DoubleToNumberConverter.New(this.targetType);
			}
		}
	}
}

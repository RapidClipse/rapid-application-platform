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

package software.xdev.rap.server.data.converter;


import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.DateFormatSymbols;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.FormatStyle;
import java.time.temporal.Temporal;
import java.util.Currency;
import java.util.Date;
import java.util.Locale;

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
	public Converter<PRESENTATION, MODEL> build();


	@SuppressWarnings("unchecked")
	public static <MODEL extends Number> StringToNumberConverterBuilder<MODEL> stringToNumber(
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
		return new StringToNumberConverterBuilder<>(Byte.class);
	}


	public static StringToNumberConverterBuilder<Short> StringToShort()
	{
		return new StringToNumberConverterBuilder<>(Short.class);
	}


	public static StringToNumberConverterBuilder<Integer> StringToInteger()
	{
		return new StringToNumberConverterBuilder<>(Integer.class);
	}


	public static StringToNumberConverterBuilder<Long> StringToLong()
	{
		return new StringToNumberConverterBuilder<>(Long.class);
	}


	public static StringToNumberConverterBuilder<BigInteger> StringToBigInteger()
	{
		return new StringToNumberConverterBuilder<>(Integer.class);
	}


	public static StringToNumberConverterBuilder<Float> StringToFloat()
	{
		return new StringToNumberConverterBuilder<>(Float.class);
	}


	public static StringToNumberConverterBuilder<Double> StringToDouble()
	{
		return new StringToNumberConverterBuilder<>(Double.class);
	}


	public static StringToNumberConverterBuilder<BigDecimal> StringToBigDecimal()
	{
		return new StringToNumberConverterBuilder<>(BigDecimal.class);
	}


	public static StringToBooleanConverterBuilder StringToBoolean()
	{
		return new StringToBooleanConverterBuilder();
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
		return new StringToDateConverterBuilder<>(Date.class);
	}


	public static StringToDateConverterBuilder<java.sql.Date> StringToSqlDate()
	{
		return new StringToDateConverterBuilder<>(java.sql.Date.class);
	}


	public static StringToDateConverterBuilder<java.sql.Time> StringToSqlTime()
	{
		return new StringToDateConverterBuilder<>(java.sql.Time.class);
	}


	public static StringToDateConverterBuilder<java.sql.Timestamp> StringToSqlTimestamp()
	{
		return new StringToDateConverterBuilder<>(java.sql.Timestamp.class);
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
		return new StringToTemporalConverterBuilder<>(LocalDate.class);
	}


	public static StringToTemporalConverterBuilder<LocalTime> StringToLocalTime()
	{
		return new StringToTemporalConverterBuilder<>(LocalTime.class);
	}


	public static StringToTemporalConverterBuilder<LocalDateTime> StringToLocalDateTime()
	{
		return new StringToTemporalConverterBuilder<>(LocalDateTime.class);
	}


	public static StringToTemporalConverterBuilder<OffsetTime> StringToOffsetTime()
	{
		return new StringToTemporalConverterBuilder<>(OffsetTime.class);
	}


	public static StringToTemporalConverterBuilder<OffsetDateTime> StringToOffsetDateTime()
	{
		return new StringToTemporalConverterBuilder<>(OffsetDateTime.class);
	}


	public static StringToTemporalConverterBuilder<ZonedDateTime> StringToZonedDateTime()
	{
		return new StringToTemporalConverterBuilder<>(ZonedDateTime.class);
	}


	public static StringToTemporalConverterBuilder<Year> StringToYear()
	{
		return new StringToTemporalConverterBuilder<>(Year.class);
	}


	public static StringToTemporalConverterBuilder<YearMonth> StringToYearMonth()
	{
		return new StringToTemporalConverterBuilder<>(YearMonth.class);
	}



	public static class Static
	{
		public static Locale getLocale(final Locale thisLocale, final Locale localeParam)
		{
			// user set locale has priority
			if(thisLocale != null)
			{
				return thisLocale;
			}
			// context's locale
			else if(localeParam != null)
			{
				return localeParam;
			}
			// use default
			return Locale.getDefault(Locale.Category.FORMAT);
		}
	}



	public static class StringToNumberConverterBuilder<MODEL extends Number>
			implements ConverterBuilder<String, MODEL>
	{
		private static enum FormatType
		{
			INTEGER, NUMBER, CURRENCY, PERCENT
		}

		private final Class<? extends Number>	targetType;

		private ErrorMessageProvider			errorMessageProvider	= context -> "Conversion error";

		private Locale							locale;
		private FormatType						formatType;

		// NumberFormat
		private Boolean							groupingUsed;
		private Integer							maximumIntegerDigits;
		private Integer							minimumIntegerDigits;
		private Integer							maximumFractionDigits;
		private Integer							minimumFractionDigits;
		private Currency						currency;
		private String							currencySymbol;
		private RoundingMode					roundingMode;

		// DecimalFormat
		private DecimalFormatSymbols			decimalFormatSymbols;
		private Boolean							decimalSeparatorAlwaysShown;
		private Integer							groupingSize;
		private Integer							multiplier;
		private String							negativePrefix;
		private String							negativeSuffix;
		private String							positivePrefix;
		private String							positiveSuffix;

		private MODEL							emptyValue;


		StringToNumberConverterBuilder(final Class<? extends Number> targetType)
		{
			this.targetType = targetType;

			if(Byte.class.equals(targetType) || Short.class.equals(targetType)
					|| Integer.class.equals(targetType) || Long.class.equals(targetType))
			{
				this.formatType = FormatType.INTEGER;
			}
			else
			{
				this.formatType = FormatType.NUMBER;
			}
		}


		public StringToNumberConverterBuilder<MODEL> errorMessage(final String errorMessage)
		{
			this.errorMessageProvider = context -> errorMessage;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> errorMessageProvider(
				final ErrorMessageProvider errorMessageProvider)
		{
			this.errorMessageProvider = errorMessageProvider;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> locale(final Locale locale)
		{
			this.locale = locale;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> currency()
		{
			this.formatType = FormatType.CURRENCY;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> percent()
		{
			this.formatType = FormatType.PERCENT;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> groupingUsed(final boolean groupingUsed)
		{
			this.groupingUsed = groupingUsed;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> maximumIntegerDigits(
				final int maximumIntegerDigits)
		{
			this.maximumIntegerDigits = maximumIntegerDigits;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> minimumIntegerDigits(
				final int minimumIntegerDigits)
		{
			this.minimumIntegerDigits = minimumIntegerDigits;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> maximumFractionDigits(
				final int maximumFractionDigits)
		{
			this.maximumFractionDigits = maximumFractionDigits;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> minimumFractionDigits(
				final int minimumFractionDigits)
		{
			this.minimumFractionDigits = minimumFractionDigits;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> currency(final Currency currency)
		{
			currency(); // set to currency format
			this.currency = currency;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> currencySymbol(final String currencySymbol)
		{
			currency(); // set to currency format
			this.currencySymbol = currencySymbol;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> decimalFormatSymbols(
				final DecimalFormatSymbols decimalFormatSymbols)
		{
			this.decimalFormatSymbols = decimalFormatSymbols;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> roundingMode(final RoundingMode roundingMode)
		{
			this.roundingMode = roundingMode;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> decimalSeparatorAlwaysShown(
				final boolean decimalSeparatorAlwaysShown)
		{
			this.decimalSeparatorAlwaysShown = decimalSeparatorAlwaysShown;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> groupingSize(final int groupingSize)
		{
			this.groupingSize = groupingSize;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> multiplier(final int multiplier)
		{
			this.multiplier = multiplier;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> negativePrefix(final String negativePrefix)
		{
			this.negativePrefix = negativePrefix;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> negativeSuffix(final String negativeSuffix)
		{
			this.negativeSuffix = negativeSuffix;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> positivePrefix(final String positivePrefix)
		{
			this.positivePrefix = positivePrefix;
			return this;
		}


		public StringToNumberConverterBuilder<MODEL> positiveSuffix(final String positiveSuffix)
		{
			this.positiveSuffix = positiveSuffix;
			return this;
		}


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
						return StringToNumberConverterBuilder.this.getFormat(locale);
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
						return StringToNumberConverterBuilder.this.getFormat(locale);
					}
				};
			}

			if(Integer.class.equals(this.targetType))
			{
				return (Converter<String, MODEL>)new StringToIntegerConverter(
						(Integer)this.emptyValue,this.errorMessageProvider)
				{
					@Override
					protected NumberFormat getFormat(final Locale locale)
					{
						return StringToNumberConverterBuilder.this.getFormat(locale);
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
						return StringToNumberConverterBuilder.this.getFormat(locale);
					}
				};
			}

			if(BigInteger.class.equals(this.targetType))
			{
				return (Converter<String, MODEL>)new StringToBigIntegerConverter(
						(BigInteger)this.emptyValue,this.errorMessageProvider)
				{
					@Override
					protected NumberFormat getFormat(final Locale locale)
					{
						final NumberFormat format = StringToNumberConverterBuilder.this
								.getFormat(locale);
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
						return StringToNumberConverterBuilder.this.getFormat(locale);
					}
				};
			}

			if(Double.class.equals(this.targetType))
			{
				return (Converter<String, MODEL>)new StringToDoubleConverter(
						(Double)this.emptyValue,this.errorMessageProvider)
				{
					@Override
					protected NumberFormat getFormat(final Locale locale)
					{
						return StringToNumberConverterBuilder.this.getFormat(locale);
					}
				};
			}

			if(BigDecimal.class.equals(this.targetType))
			{
				return (Converter<String, MODEL>)new StringToBigDecimalConverter(
						(BigDecimal)this.emptyValue,this.errorMessageProvider)
				{
					@Override
					protected NumberFormat getFormat(final Locale locale)
					{
						final NumberFormat format = StringToNumberConverterBuilder.this
								.getFormat(locale);
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


		private NumberFormat getFormat(final Locale localeParam)
		{
			final Locale locale = Static.getLocale(this.locale,localeParam);

			NumberFormat format = null;

			switch(this.formatType)
			{
				case INTEGER:
					format = NumberFormat.getIntegerInstance(locale);
				break;

				case NUMBER:
					format = NumberFormat.getNumberInstance(locale);
				break;

				case CURRENCY:
					format = NumberFormat.getCurrencyInstance(locale);
				break;

				case PERCENT:
					format = NumberFormat.getPercentInstance(locale);
				break;
			}

			if(this.groupingUsed != null)
			{
				format.setGroupingUsed(this.groupingUsed);
			}
			if(this.maximumIntegerDigits != null)
			{
				format.setMaximumIntegerDigits(this.maximumIntegerDigits);
			}
			if(this.minimumIntegerDigits != null)
			{
				format.setMinimumIntegerDigits(this.minimumIntegerDigits);
			}
			if(this.maximumFractionDigits != null)
			{
				format.setMaximumFractionDigits(this.maximumFractionDigits);
			}
			if(this.minimumFractionDigits != null)
			{
				format.setMinimumFractionDigits(this.minimumFractionDigits);
			}
			if(this.currency != null)
			{
				format.setCurrency(this.currency);
			}
			if(this.roundingMode != null)
			{
				format.setRoundingMode(this.roundingMode);
			}

			if(format instanceof DecimalFormat)
			{
				final DecimalFormat decimalFormat = (DecimalFormat)format;

				if(this.decimalFormatSymbols != null)
				{
					decimalFormat.setDecimalFormatSymbols(this.decimalFormatSymbols);
				}
				if(this.currencySymbol != null)
				{
					decimalFormat.getDecimalFormatSymbols().setCurrencySymbol(this.currencySymbol);
				}
				if(this.decimalSeparatorAlwaysShown != null)
				{
					decimalFormat.setDecimalSeparatorAlwaysShown(this.decimalSeparatorAlwaysShown);
				}
				if(this.groupingSize != null)
				{
					decimalFormat.setGroupingSize(this.groupingSize);
				}
				if(this.multiplier != null)
				{
					decimalFormat.setMultiplier(this.multiplier);
				}
				if(this.negativePrefix != null)
				{
					decimalFormat.setNegativePrefix(this.negativePrefix);
				}
				if(this.negativeSuffix != null)
				{
					decimalFormat.setNegativeSuffix(this.negativeSuffix);
				}
				if(this.positivePrefix != null)
				{
					decimalFormat.setPositivePrefix(this.positivePrefix);
				}
				if(this.positiveSuffix != null)
				{
					decimalFormat.setPositiveSuffix(this.positiveSuffix);
				}
			}

			return format;
		}
	}



	public static class StringToBooleanConverterBuilder implements ConverterBuilder<String, Boolean>
	{
		private ErrorMessageProvider	errorMessageProvider	= context -> "Conversion error";

		private String					trueString				= Boolean.TRUE.toString();
		private String					falseString				= Boolean.FALSE.toString();


		StringToBooleanConverterBuilder()
		{
		}


		public StringToBooleanConverterBuilder errorMessage(final String errorMessage)
		{
			this.errorMessageProvider = context -> errorMessage;
			return this;
		}


		public StringToBooleanConverterBuilder errorMessageProvider(
				final ErrorMessageProvider errorMessageProvider)
		{
			this.errorMessageProvider = errorMessageProvider;
			return this;
		}


		public StringToBooleanConverterBuilder trueString(final String trueString)
		{
			this.trueString = trueString;
			return this;
		}


		public StringToBooleanConverterBuilder falseString(final String falseString)
		{
			this.falseString = falseString;
			return this;
		}


		@Override
		public Converter<String, Boolean> build()
		{
			return new StringToBooleanConverter(this.trueString,this.falseString,
					this.errorMessageProvider);
		}
	}



	public static class StringToDateConverterBuilder<MODEL extends Date>
			implements ConverterBuilder<String, MODEL>
	{
		private final Class<MODEL>	targetType;
		private Locale				locale;
		private boolean				date		= true;
		private boolean				time		= true;
		private int					dateStyle	= DateFormat.DEFAULT;
		private int					timeStyle	= DateFormat.DEFAULT;
		private String				pattern;
		private DateFormatSymbols	dateFormatSymbols;


		StringToDateConverterBuilder(final Class<MODEL> targetType)
		{
			super();

			this.targetType = targetType;

			if(java.sql.Time.class.equals(targetType))
			{
				this.date = false;
			}
		}


		public StringToDateConverterBuilder<MODEL> locale(final Locale locale)
		{
			this.locale = locale;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> dateOnly()
		{
			this.date = true;
			this.time = false;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> timeOnly()
		{
			this.date = false;
			this.time = true;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> dateAndTime()
		{
			this.date = true;
			this.time = true;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> dateStyleShort()
		{
			this.dateStyle = DateFormat.SHORT;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> dateStyleMedium()
		{
			this.dateStyle = DateFormat.MEDIUM;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> dateStyleLong()
		{
			this.dateStyle = DateFormat.LONG;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> dateStyleFull()
		{
			this.dateStyle = DateFormat.FULL;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> dateStyleDefault()
		{
			this.dateStyle = DateFormat.DEFAULT;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> timeStyleShort()
		{
			this.timeStyle = DateFormat.SHORT;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> timeStyleMedium()
		{
			this.timeStyle = DateFormat.MEDIUM;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> timeStyleLong()
		{
			this.timeStyle = DateFormat.LONG;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> timeStyleFull()
		{
			this.timeStyle = DateFormat.FULL;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> timeStyleDefault()
		{
			this.timeStyle = DateFormat.DEFAULT;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> pattern(final String pattern)
		{
			this.pattern = pattern;
			return this;
		}


		public StringToDateConverterBuilder<MODEL> dateFormatSymbols(
				final DateFormatSymbols dateFormatSymbols)
		{
			this.dateFormatSymbols = dateFormatSymbols;
			return this;
		}


		private DateFormat getFormat(final Locale localeParam)
		{
			final Locale locale = Static.getLocale(this.locale,localeParam);

			if(this.pattern != null)
			{
				if(this.dateFormatSymbols != null)
				{
					return new SimpleDateFormat(this.pattern,this.dateFormatSymbols);
				}
				return new SimpleDateFormat(this.pattern,locale);
			}

			if(this.date && this.time)
			{
				return DateFormat.getDateTimeInstance(this.dateStyle,this.timeStyle,locale);
			}
			if(this.date)
			{
				return DateFormat.getDateInstance(this.dateStyle,locale);
			}
			return DateFormat.getTimeInstance(this.timeStyle,locale);
		}


		@Override
		public Converter<String, MODEL> build()
		{
			return StringToDateConverter.New(this.targetType,this::getFormat);
		}
	}



	public static class StringToTemporalConverterBuilder<MODEL extends Temporal>
			implements ConverterBuilder<String, MODEL>
	{
		private final Class<MODEL>	targetType;
		private Locale				locale;
		private boolean				date		= true;
		private boolean				time		= true;
		private FormatStyle			dateStyle	= FormatStyle.MEDIUM;
		private FormatStyle			timeStyle	= FormatStyle.MEDIUM;
		private String				pattern;


		StringToTemporalConverterBuilder(final Class<MODEL> targetType)
		{
			super();

			this.targetType = targetType;

			if(LocalTime.class.equals(targetType) || OffsetTime.class.equals(targetType)
					|| LocalTime.class.equals(targetType))
			{
				this.date = false;
			}
			else if(LocalDate.class.equals(targetType) || Year.class.equals(targetType)
					|| YearMonth.class.equals(targetType))
			{
				this.time = false;
			}
		}


		public StringToTemporalConverterBuilder<MODEL> locale(final Locale locale)
		{
			this.locale = locale;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> dateOnly()
		{
			this.date = true;
			this.time = false;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> timeOnly()
		{
			this.date = false;
			this.time = true;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> dateAndTime()
		{
			this.date = true;
			this.time = true;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> dateStyleShort()
		{
			this.dateStyle = FormatStyle.SHORT;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> dateStyleMedium()
		{
			this.dateStyle = FormatStyle.MEDIUM;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> dateStyleLong()
		{
			this.dateStyle = FormatStyle.LONG;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> dateStyleFull()
		{
			this.dateStyle = FormatStyle.FULL;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> dateStyleDefault()
		{
			this.dateStyle = FormatStyle.MEDIUM;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> timeStyleShort()
		{
			this.timeStyle = FormatStyle.SHORT;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> timeStyleMedium()
		{
			this.timeStyle = FormatStyle.MEDIUM;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> timeStyleLong()
		{
			this.timeStyle = FormatStyle.LONG;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> timeStyleFull()
		{
			this.timeStyle = FormatStyle.FULL;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> timeStyleDefault()
		{
			this.timeStyle = FormatStyle.MEDIUM;
			return this;
		}


		public StringToTemporalConverterBuilder<MODEL> pattern(final String pattern)
		{
			this.pattern = pattern;
			return this;
		}


		private DateTimeFormatter getFormatter(final Locale localeParam)
		{
			final Locale locale = Static.getLocale(this.locale,localeParam);

			if(this.pattern != null)
			{
				return DateTimeFormatter.ofPattern(pattern,locale);
			}

			if(this.date && this.time)
			{
				return new DateTimeFormatterBuilder().appendLocalized(dateStyle,timeStyle)
						.toFormatter(locale);
			}
			if(this.date)
			{
				return new DateTimeFormatterBuilder().appendLocalized(dateStyle,null)
						.toFormatter(locale);
			}
			return new DateTimeFormatterBuilder().appendLocalized(null,timeStyle)
					.toFormatter(locale);
		}


		@Override
		public Converter<String, MODEL> build()
		{
			return StringToTemporalConverter.New(this.targetType,this::getFormatter);
		}
	}
}
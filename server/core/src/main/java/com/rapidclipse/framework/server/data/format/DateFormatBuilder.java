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

package com.rapidclipse.framework.server.data.format;

import static java.util.Objects.requireNonNull;

import java.text.DateFormat;
import java.text.DateFormatSymbols;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;


/**
 * Builder for the {@link DateFormat} type, simply to introduce a builder pattern.
 *
 * @author XDEV Software
 *
 */
public interface DateFormatBuilder<B extends DateFormatBuilder<B>>
{
	public static DateOnlyFormatBuilder Date()
	{
		return new DateOnlyFormatBuilder.Implementation();
	}
	
	public static TimeOnlyFormatBuilder Time()
	{
		return new TimeOnlyFormatBuilder.Implementation();
	}
	
	public static DateTimeFormatBuilder DateTime()
	{
		return new DateTimeFormatBuilder.Implementation();
	}
	
	public static SimpleDateFormatBuilder Simple(final String pattern)
	{
		return new SimpleDateFormatBuilder.Implementation(pattern);
	}
	
	public B locale(Locale locale);
	
	/**
	 * @see DateFormat#setCalendar(Calendar)
	 */
	public B calendar(Calendar calendar);
	
	/**
	 * @see DateFormat#setLenient(boolean)
	 */
	public B lenient(boolean lenient);
	
	/**
	 * @see DateFormat#setNumberFormat(NumberFormat)
	 */
	public B numberFormat(NumberFormat numberFormat);
	
	/**
	 * @see DateFormat#setTimeZone(TimeZone)
	 */
	public B timeZone(TimeZone timeZone);
	
	public DateFormat build();
	
	public static abstract class Abstract<B extends DateFormatBuilder<B>> implements DateFormatBuilder<B>
	{
		private Locale       locale;
		private Calendar     calendar;
		private Boolean      lenient;
		private NumberFormat numberFormat;
		private TimeZone     timeZone;
		
		protected Abstract()
		{
			super();
		}
		
		@Override
		public B locale(final Locale locale)
		{
			this.locale = locale;
			return $this();
		}
		
		@Override
		public B calendar(final Calendar calendar)
		{
			this.calendar = calendar;
			return $this();
		}
		
		@Override
		public B lenient(final boolean lenient)
		{
			this.lenient = lenient;
			return $this();
		}
		
		@Override
		public B numberFormat(final NumberFormat numberFormat)
		{
			this.numberFormat = numberFormat;
			return $this();
		}
		
		@Override
		public B timeZone(final TimeZone timeZone)
		{
			this.timeZone = timeZone;
			return $this();
		}
		
		@Override
		public DateFormat build()
		{
			final DateFormat format = createFormat(this.locale);
			updateFormat(format);
			return format;
		}
		
		protected abstract DateFormat createFormat(Locale locale);
		
		protected void updateFormat(final DateFormat format)
		{
			if(this.calendar != null)
			{
				format.setCalendar(this.calendar);
			}
			if(this.lenient != null)
			{
				format.setLenient(this.lenient);
			}
			if(this.numberFormat != null)
			{
				format.setNumberFormat(this.numberFormat);
			}
			if(this.timeZone != null)
			{
				format.setTimeZone(this.timeZone);
			}
		}
		
		@SuppressWarnings("unchecked")
		protected B $this()
		{
			return (B)this;
		}
	}
	
	public interface DateOnlyFormatBuilder extends DateFormatBuilder<DateOnlyFormatBuilder>
	{
		/**
		 * @see DateFormat#DEFAULT
		 * @see DateFormat#SHORT
		 * @see DateFormat#MEDIUM
		 * @see DateFormat#LONG
		 * @see DateFormat#FULL
		 */
		public DateOnlyFormatBuilder dateStyle(int dateStyle);
		
		public static class Implementation extends Abstract<DateOnlyFormatBuilder>
			implements DateOnlyFormatBuilder
		{
			private int dateStyle = DateFormat.DEFAULT;
			
			protected Implementation()
			{
				super();
			}
			
			@Override
			public DateOnlyFormatBuilder dateStyle(final int dateStyle)
			{
				if(dateStyle < 0 || dateStyle > 3)
				{
					throw new IllegalArgumentException("Illegal date style");
				}
				this.dateStyle = dateStyle;
				return $this();
			}
			
			@Override
			protected DateFormat createFormat(final Locale locale)
			{
				return locale != null ? DateFormat.getDateInstance(this.dateStyle, locale)
					: DateFormat.getDateInstance(this.dateStyle);
			}
		}
	}
	
	public interface TimeOnlyFormatBuilder extends DateFormatBuilder<TimeOnlyFormatBuilder>
	{
		/**
		 * @see DateFormat#DEFAULT
		 * @see DateFormat#SHORT
		 * @see DateFormat#MEDIUM
		 * @see DateFormat#LONG
		 * @see DateFormat#FULL
		 */
		public TimeOnlyFormatBuilder timeStyle(int timeStyle);
		
		public static class Implementation extends Abstract<TimeOnlyFormatBuilder>
			implements TimeOnlyFormatBuilder
		{
			private int timeStyle = DateFormat.DEFAULT;
			
			protected Implementation()
			{
				super();
			}
			
			@Override
			public TimeOnlyFormatBuilder timeStyle(final int timeStyle)
			{
				if(timeStyle < 0 || timeStyle > 3)
				{
					throw new IllegalArgumentException("Illegal time style");
				}
				this.timeStyle = timeStyle;
				return $this();
			}
			
			@Override
			protected DateFormat createFormat(final Locale locale)
			{
				return locale != null ? DateFormat.getTimeInstance(this.timeStyle, locale)
					: DateFormat.getTimeInstance(this.timeStyle);
			}
		}
	}
	
	public interface DateTimeFormatBuilder extends DateFormatBuilder<DateTimeFormatBuilder>
	{
		/**
		 * @see DateFormat#DEFAULT
		 * @see DateFormat#SHORT
		 * @see DateFormat#MEDIUM
		 * @see DateFormat#LONG
		 * @see DateFormat#FULL
		 */
		public DateTimeFormatBuilder dateStyle(int dateStyle);
		
		/**
		 * @see DateFormat#DEFAULT
		 * @see DateFormat#SHORT
		 * @see DateFormat#MEDIUM
		 * @see DateFormat#LONG
		 * @see DateFormat#FULL
		 */
		public DateTimeFormatBuilder timeStyle(int timeStyle);
		
		public static class Implementation extends Abstract<DateTimeFormatBuilder>
			implements DateTimeFormatBuilder
		{
			private int dateStyle = DateFormat.DEFAULT;
			private int timeStyle = DateFormat.DEFAULT;
			
			protected Implementation()
			{
				super();
			}
			
			@Override
			public DateTimeFormatBuilder dateStyle(final int dateStyle)
			{
				if(dateStyle < 0 || dateStyle > 3)
				{
					throw new IllegalArgumentException("Illegal date style");
				}
				this.dateStyle = dateStyle;
				return $this();
			}
			
			@Override
			public DateTimeFormatBuilder timeStyle(final int timeStyle)
			{
				if(timeStyle < 0 || timeStyle > 3)
				{
					throw new IllegalArgumentException("Illegal time style");
				}
				this.timeStyle = timeStyle;
				return $this();
			}
			
			@Override
			protected DateFormat createFormat(final Locale locale)
			{
				return locale != null ? DateFormat.getDateTimeInstance(this.dateStyle, this.timeStyle, locale)
					: DateFormat.getDateTimeInstance(this.dateStyle, this.timeStyle);
			}
		}
	}
	
	public interface SimpleDateFormatBuilder extends DateFormatBuilder<SimpleDateFormatBuilder>
	{
		/**
		 * @see SimpleDateFormat#setDateFormatSymbols(DateFormatSymbols)
		 */
		public SimpleDateFormatBuilder dateFormatSymbols(DateFormatSymbols dateFormatSymbols);
		
		/**
		 * @see SimpleDateFormat#set2DigitYearStart(Date)
		 */
		public SimpleDateFormatBuilder twoDigitYearStart(Date twoDigitYearStart);
		
		public static class Implementation extends Abstract<SimpleDateFormatBuilder>
			implements SimpleDateFormatBuilder
		{
			private final String      pattern;
			private DateFormatSymbols dateFormatSymbols;
			private Date              twoDigitYearStart;
			
			protected Implementation(final String pattern)
			{
				super();
				this.pattern = requireNonNull(pattern);
			}
			
			@Override
			public SimpleDateFormatBuilder dateFormatSymbols(final DateFormatSymbols dateFormatSymbols)
			{
				this.dateFormatSymbols = dateFormatSymbols;
				return $this();
			}
			
			@Override
			public SimpleDateFormatBuilder twoDigitYearStart(final Date twoDigitYearStart)
			{
				this.twoDigitYearStart = twoDigitYearStart;
				return $this();
			}
			
			@Override
			protected DateFormat createFormat(final Locale locale)
			{
				return locale != null ? new SimpleDateFormat(this.pattern, locale) : new SimpleDateFormat(this.pattern);
			}
			
			@Override
			protected void updateFormat(final DateFormat format)
			{
				super.updateFormat(format);
				
				if(this.dateFormatSymbols != null)
				{
					((SimpleDateFormat)format).setDateFormatSymbols(this.dateFormatSymbols);
				}
				if(this.twoDigitYearStart != null)
				{
					((SimpleDateFormat)format).set2DigitYearStart(this.twoDigitYearStart);
				}
			}
		}
	}
}

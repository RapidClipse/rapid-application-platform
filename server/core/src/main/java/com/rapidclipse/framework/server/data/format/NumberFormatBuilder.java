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
package com.rapidclipse.framework.server.data.format;

import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Currency;
import java.util.Locale;
import java.util.Optional;


/**
 * Builder for the {@link NumberFormat} type, simply to introduce a builder pattern.
 *
 * @author XDEV Software
 *
 */
public interface NumberFormatBuilder<B extends NumberFormatBuilder<B>>
{
	public static IntegerFormatBuilder Integer()
	{
		return new IntegerFormatBuilder.Default();
	}
	
	public static DecimalFormatBuilder Decimal()
	{
		return new DecimalFormatBuilder.Default();
	}
	
	public static PercentFormatBuilder Percent()
	{
		return new PercentFormatBuilder.Default();
	}
	
	public static CurrencyFormatBuilder Currency()
	{
		return new CurrencyFormatBuilder.Default();
	}
	
	public B locale(Locale locale);
	
	/**
	 * @see DecimalFormat#applyPattern(String)
	 */
	public B pattern(String pattern);
	
	/**
	 * @see DecimalFormat#applyLocalizedPattern(String)
	 */
	public B localizedPattern(String localizedPattern);
	
	/**
	 * @see NumberFormat#setGroupingUsed(boolean)
	 */
	public B groupingUsed(boolean groupingUsed);
	
	/**
	 * @see DecimalFormat#setGroupingSize(int)
	 */
	public B groupingSize(int groupingSize);
	
	/**
	 * @see NumberFormat#setMaximumIntegerDigits(int)
	 */
	public B maximumIntegerDigits(int maximumIntegerDigits);
	
	/**
	 * @see NumberFormat#setMinimumIntegerDigits(int)
	 */
	public B minimumIntegerDigits(int minimumIntegerDigits);
	
	/**
	 * @see NumberFormat#setMaximumFractionDigits(int)
	 */
	public B maximumFractionDigits(int maximumFractionDigits);
	
	/**
	 * @see NumberFormat#setMinimumFractionDigits(int)
	 */
	public B minimumFractionDigits(int minimumFractionDigits);
	
	/**
	 * @see NumberFormat#setRoundingMode(RoundingMode)
	 */
	public B roundingMode(RoundingMode roundingMode);
	
	/**
	 * @see DecimalFormat#setDecimalSeparatorAlwaysShown(boolean)
	 */
	public B decimalSeparatorAlwaysShown(boolean decimalSeparatorAlwaysShown);
	
	/**
	 * @see DecimalFormat#setMultiplier(int)
	 */
	public B multiplier(int multiplier);

	/**
	 * @see DecimalFormat#setNegativePrefix(String)
	 */
	public B negativePrefix(String negativePrefix);

	/**
	 * @see DecimalFormat#setNegativeSuffix(String)
	 */
	public B negativeSuffix(String negativeSuffix);

	/**
	 * @see DecimalFormat#setPositivePrefix(String)
	 */
	public B positivePrefix(String positivePrefix);

	/**
	 * @see DecimalFormat#setPositiveSuffix(String)
	 */
	public B positiveSuffix(String positiveSuffix);
	
	public NumberFormat build();
	
	public static abstract class Abstract<B extends NumberFormatBuilder<B>> implements NumberFormatBuilder<B>
	{
		private Locale       locale;
		private String       pattern;
		private String       localizedPattern;
		private Boolean      groupingUsed;
		private Integer      groupingSize;
		private Integer      maximumIntegerDigits;
		private Integer      minimumIntegerDigits;
		private Integer      maximumFractionDigits;
		private Integer      minimumFractionDigits;
		private RoundingMode roundingMode;
		private Boolean      decimalSeparatorAlwaysShown;
		private Integer      multiplier;
		private String       negativePrefix;
		private String       negativeSuffix;
		private String       positivePrefix;
		private String       positiveSuffix;
		
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
		public B pattern(final String pattern)
		{
			this.pattern          = pattern;
			this.localizedPattern = null;
			return $this();
		}
		
		@Override
		public B localizedPattern(final String localizedPattern)
		{
			this.pattern          = null;
			this.localizedPattern = localizedPattern;
			return $this();
		}
		
		@Override
		public B groupingUsed(final boolean groupingUsed)
		{
			this.groupingUsed = groupingUsed;
			return $this();
		}
		
		@Override
		public B groupingSize(final int groupingSize)
		{
			this.groupingSize = groupingSize;
			return $this();
		}
		
		@Override
		public B maximumIntegerDigits(final int maximumIntegerDigits)
		{
			this.maximumIntegerDigits = maximumIntegerDigits;
			return $this();
		}
		
		@Override
		public B minimumIntegerDigits(final int minimumIntegerDigits)
		{
			this.minimumIntegerDigits = minimumIntegerDigits;
			return $this();
		}
		
		@Override
		public B maximumFractionDigits(final int maximumFractionDigits)
		{
			this.maximumFractionDigits = maximumFractionDigits;
			return $this();
		}
		
		@Override
		public B minimumFractionDigits(final int minimumFractionDigits)
		{
			this.minimumFractionDigits = minimumFractionDigits;
			return $this();
		}
		
		@Override
		public B roundingMode(final RoundingMode roundingMode)
		{
			this.roundingMode = roundingMode;
			return $this();
		}
		
		@Override
		public B decimalSeparatorAlwaysShown(final boolean decimalSeparatorAlwaysShown)
		{
			this.decimalSeparatorAlwaysShown = decimalSeparatorAlwaysShown;
			return $this();
		}
		
		@Override
		public B multiplier(final int multiplier)
		{
			this.multiplier = multiplier;
			return $this();
		}
		
		@Override
		public B negativePrefix(final String negativePrefix)
		{
			this.negativePrefix = negativePrefix;
			return $this();
		}
		
		@Override
		public B negativeSuffix(final String negativeSuffix)
		{
			this.negativeSuffix = negativeSuffix;
			return $this();
		}
		
		@Override
		public B positivePrefix(final String positivePrefix)
		{
			this.positivePrefix = positivePrefix;
			return $this();
		}
		
		@Override
		public B positiveSuffix(final String positiveSuffix)
		{
			this.positiveSuffix = positiveSuffix;
			return $this();
		}
		
		@Override
		public NumberFormat build()
		{
			final NumberFormat format = createFormat(this.locale);
			updateFormat(format);
			return format;
		}
		
		protected abstract NumberFormat createFormat(Locale locale);
		
		protected void updateFormat(final NumberFormat format)
		{
			if(this.pattern != null)
			{
				decimalFormat(format).ifPresent(df -> df.applyPattern(this.pattern));
			}
			else if(this.localizedPattern != null)
			{
				decimalFormat(format).ifPresent(df -> df.applyLocalizedPattern(this.localizedPattern));
			}
			if(this.groupingUsed != null)
			{
				format.setGroupingUsed(this.groupingUsed);
			}
			if(this.groupingSize != null)
			{
				decimalFormat(format).ifPresent(df -> df.setGroupingSize(this.groupingSize));
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
			if(this.roundingMode != null)
			{
				format.setRoundingMode(this.roundingMode);
			}
			if(this.decimalSeparatorAlwaysShown != null)
			{
				decimalFormat(format)
					.ifPresent(df -> df.setDecimalSeparatorAlwaysShown(this.decimalSeparatorAlwaysShown));
			}
			if(this.multiplier != null)
			{
				decimalFormat(format).ifPresent(df -> df.setMultiplier(this.multiplier));
			}
			if(this.negativePrefix != null)
			{
				decimalFormat(format).ifPresent(df -> df.setNegativePrefix(this.negativePrefix));
			}
			if(this.negativeSuffix != null)
			{
				decimalFormat(format).ifPresent(df -> df.setNegativeSuffix(this.negativeSuffix));
			}
			if(this.positivePrefix != null)
			{
				decimalFormat(format).ifPresent(df -> df.setPositivePrefix(this.positivePrefix));
			}
			if(this.positiveSuffix != null)
			{
				decimalFormat(format).ifPresent(df -> df.setPositiveSuffix(this.positiveSuffix));
			}
		}
		
		protected Optional<DecimalFormat> decimalFormat(final NumberFormat format)
		{
			return format instanceof DecimalFormat ? Optional.of((DecimalFormat)format) : Optional.empty();
		}
		
		@SuppressWarnings("unchecked")
		protected B $this()
		{
			return (B)this;
		}
	}
	
	public interface IntegerFormatBuilder extends NumberFormatBuilder<IntegerFormatBuilder>
	{
		public static class Default extends Abstract<IntegerFormatBuilder>
			implements IntegerFormatBuilder
		{
			protected Default()
			{
				super();
			}
			
			@Override
			protected NumberFormat createFormat(final Locale locale)
			{
				return locale != null ? NumberFormat.getIntegerInstance(locale)
					: NumberFormat.getIntegerInstance();
			}
		}
	}
	
	public interface FloatingFormatBuilder<B extends FloatingFormatBuilder<B>> extends NumberFormatBuilder<B>
	{
		/**
		 * @see DecimalFormat#setDecimalFormatSymbols(DecimalFormatSymbols)
		 */
		public B decimalFormatSymbols(DecimalFormatSymbols decimalFormatSymbols);
		
		public static abstract class Abstract<B extends FloatingFormatBuilder<B>>
			extends NumberFormatBuilder.Abstract<B>
			implements FloatingFormatBuilder<B>
		{
			private DecimalFormatSymbols decimalFormatSymbols;
			
			protected Abstract()
			{
				super();
			}
			
			@Override
			public B decimalFormatSymbols(final DecimalFormatSymbols decimalFormatSymbols)
			{
				this.decimalFormatSymbols = decimalFormatSymbols;
				return $this();
			}
			
			@Override
			protected void updateFormat(final NumberFormat format)
			{
				super.updateFormat(format);
				
				if(this.decimalFormatSymbols != null)
				{
					decimalFormat(format).ifPresent(df -> df.setDecimalFormatSymbols(this.decimalFormatSymbols));
				}
			}
		}
	}
	
	public interface DecimalFormatBuilder extends FloatingFormatBuilder<DecimalFormatBuilder>
	{
		public static class Default extends FloatingFormatBuilder.Abstract<DecimalFormatBuilder>
			implements DecimalFormatBuilder
		{
			protected Default()
			{
				super();
			}
			
			@Override
			protected NumberFormat createFormat(final Locale locale)
			{
				return locale != null ? NumberFormat.getNumberInstance(locale)
					: NumberFormat.getNumberInstance();
			}
		}
	}
	
	public interface PercentFormatBuilder extends FloatingFormatBuilder<PercentFormatBuilder>
	{
		public static class Default extends FloatingFormatBuilder.Abstract<PercentFormatBuilder>
			implements PercentFormatBuilder
		{
			protected Default()
			{
				super();
			}
			
			@Override
			protected NumberFormat createFormat(final Locale locale)
			{
				return locale != null ? NumberFormat.getPercentInstance(locale)
					: NumberFormat.getPercentInstance();
			}
		}
	}
	
	public interface CurrencyFormatBuilder extends FloatingFormatBuilder<CurrencyFormatBuilder>
	{
		/**
		 * @see NumberFormat#setCurrency(Currency)
		 */
		public CurrencyFormatBuilder currency(Currency currency);
		
		public static class Default extends FloatingFormatBuilder.Abstract<CurrencyFormatBuilder>
			implements CurrencyFormatBuilder
		{
			private Currency currency;
			
			protected Default()
			{
				super();
			}
			
			@Override
			public CurrencyFormatBuilder currency(final Currency currency)
			{
				this.currency = currency;
				return $this();
			}
			
			@Override
			protected NumberFormat createFormat(final Locale locale)
			{
				return locale != null ? NumberFormat.getCurrencyInstance(locale)
					: NumberFormat.getCurrencyInstance();
			}
			
			@Override
			protected void updateFormat(final NumberFormat format)
			{
				super.updateFormat(format);
				
				if(this.currency != null)
				{
					format.setCurrency(this.currency);
				}
			}
		}
	}
}

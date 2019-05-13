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
		return new IntegerFormatBuilder.Implementation();
	}

	public static DecimalFormatBuilder Decimal()
	{
		return new DecimalFormatBuilder.Implementation();
	}

	public static PercentFormatBuilder Percent()
	{
		return new PercentFormatBuilder.Implementation();
	}

	public static CurrencyFormatBuilder Currency()
	{
		return new CurrencyFormatBuilder.Implementation();
	}

	public B locale(Locale locale);

	public B pattern(String pattern);

	public B localizedPattern(String localizedPattern);

	public B groupingUsed(boolean groupingUsed);

	public B groupingSize(int groupingSize);

	public B maximumIntegerDigits(int maximumIntegerDigits);

	public B minimumIntegerDigits(int minimumIntegerDigits);

	public B maximumFractionDigits(int maximumFractionDigits);

	public B minimumFractionDigits(int minimumFractionDigits);

	public B roundingMode(RoundingMode roundingMode);

	public B decimalSeparatorAlwaysShown(boolean decimalSeparatorAlwaysShown);

	public B multiplier(int multiplier);

	public B negativePrefix(String negativePrefix);

	public B negativeSuffix(String negativeSuffix);

	public B positivePrefix(String positivePrefix);

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
				decimalFormat(format).ifPresent(df -> df.setDecimalSeparatorAlwaysShown(decimalSeparatorAlwaysShown));
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
		public static class Implementation extends Abstract<IntegerFormatBuilder>
			implements IntegerFormatBuilder
		{
			protected Implementation()
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
		public static class Implementation extends FloatingFormatBuilder.Abstract<DecimalFormatBuilder>
			implements DecimalFormatBuilder
		{
			protected Implementation()
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
		public static class Implementation extends FloatingFormatBuilder.Abstract<PercentFormatBuilder>
			implements PercentFormatBuilder
		{
			protected Implementation()
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
		public CurrencyFormatBuilder currency(Currency currency);

		public static class Implementation extends FloatingFormatBuilder.Abstract<CurrencyFormatBuilder>
			implements CurrencyFormatBuilder
		{
			private Currency currency;
			
			protected Implementation()
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

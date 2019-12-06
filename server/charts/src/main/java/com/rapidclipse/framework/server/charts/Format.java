/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TimeZone;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Format extends Serializable, JavaScriptable
{
	@Override
	public default String js()
	{
		return js("format");
	}
	
	public String js(String varName);
	
	public static Format ArrowFormat(final Number base)
	{
		return varName -> "var " + varName + " = new google.visualization.ArrowFormat(" + Static.js(base) + ");";
	}
	
	public static BarFormatBuilder BarFormatBuilder()
	{
		return new BarFormatBuilder.Default();
	}
	
	public static interface BarFormatBuilder
	{
		public BarFormatBuilder base(Number base);
		
		public BarFormatBuilder colorNegative(String colorNegative);
		
		public BarFormatBuilder colorPositive(String colorPositive);
		
		public BarFormatBuilder drawZeroLine(Boolean drawZeroLine);
		
		public BarFormatBuilder max(Number max);
		
		public BarFormatBuilder min(Number min);
		
		public BarFormatBuilder showValue(Boolean showValue);
		
		public BarFormatBuilder width(Number width);
		
		public Format build();
		
		public static class Default implements BarFormatBuilder
		{
			private Number  base;
			private String  colorNegative;
			private String  colorPositive;
			private Boolean drawZeroLine;
			private Number  max;
			private Number  min;
			private Boolean showValue;
			private Number  width;
			
			Default()
			{
				super();
			}
			
			@Override
			public BarFormatBuilder base(final Number base)
			{
				this.base = base;
				return this;
			}
			
			@Override
			public BarFormatBuilder colorNegative(final String colorNegative)
			{
				this.colorNegative = colorNegative;
				return this;
			}
			
			@Override
			public BarFormatBuilder colorPositive(final String colorPositive)
			{
				this.colorPositive = colorPositive;
				return this;
			}
			
			@Override
			public BarFormatBuilder drawZeroLine(final Boolean drawZeroLine)
			{
				this.drawZeroLine = drawZeroLine;
				return this;
			}
			
			@Override
			public BarFormatBuilder max(final Number max)
			{
				this.max = max;
				return this;
			}
			
			@Override
			public BarFormatBuilder min(final Number min)
			{
				this.min = min;
				return this;
			}
			
			@Override
			public BarFormatBuilder showValue(final Boolean showValue)
			{
				this.showValue = showValue;
				return this;
			}
			
			@Override
			public BarFormatBuilder width(final Number width)
			{
				this.width = width;
				return this;
			}
			
			@Override
			public Format build()
			{
				final ObjectHelper obj = new ObjectHelper();
				obj.putIfNotNull("base", this.base);
				obj.putIfNotNull("colorNegative", this.colorNegative);
				obj.putIfNotNull("colorPositive", this.colorPositive);
				obj.putIfNotNull("drawZeroLine", this.drawZeroLine);
				obj.putIfNotNull("max", this.max);
				obj.putIfNotNull("min", this.min);
				obj.putIfNotNull("showValue", this.showValue);
				obj.putIfNotNull("width", this.width);
				return varName -> "var " + varName + " = new google.visualization.BarFormat(" + obj.js() + ");";
			}
			
		}
		
	}
	
	public static ColorFormatBuilder ColorFormatBuilder()
	{
		return new ColorFormatBuilder.Default();
	}
	
	public interface ColorFormatBuilder
	{
		public ColorFormatBuilder addRange(Object from, Object to, String color, String bgColor);
		
		public ColorFormatBuilder
			addGradientRange(Object from, Object to, String color, String fromBgColor, String toBgColor);
		
		public Format build();
		
		public static class Default implements ColorFormatBuilder
		{
			private final List<String> ranges = new ArrayList<>();
			
			Default()
			{
				super();
			}
			
			@Override
			public ColorFormatBuilder
				addRange(final Object from, final Object to, final String color, final String bgColor)
			{
				this.ranges.add("addRange("
					+ Arrays.asList(from, to, color, bgColor).stream()
						.map(Static::js).collect(Collectors.joining(","))
					+ ")");
				return this;
			}
			
			@Override
			public ColorFormatBuilder addGradientRange(
				final Object from,
				final Object to,
				final String color,
				final String fromBgColor,
				final String toBgColor)
			{
				this.ranges.add("addGradientRange("
					+ Arrays.asList(from, to, color, fromBgColor, toBgColor).stream()
						.map(Static::js).collect(Collectors.joining(","))
					+ ")");
				return null;
			}
			
			@Override
			public Format build()
			{
				return varName -> {
					final StringBuilder sb = new StringBuilder();
					sb.append("var ").append(varName).append(" = new google.visualization.ColorFormat();");
					for(final String range : this.ranges)
					{
						sb.append("\n").append(varName).append(".").append(range).append(";");
					}
					return sb.toString();
				};
			}
			
		}
		
	}
	
	public static enum FormatType implements JavaScriptable
	{
		SHORT("short"),
		MEDIUM("medium"),
		LONG("long");
		
		private final String js;
		
		private FormatType(final String js)
		{
			this.js = Json.create(js).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public static Format DateFormat(final FormatType formatType)
	{
		return DateFormat(formatType, null);
	}
	
	public static Format DateFormat(final FormatType formatType, final TimeZone timeZone)
	{
		final ObjectHelper obj = new ObjectHelper().put("formatType", formatType);
		if(timeZone != null)
		{
			final int offset = (int)Duration.ofMillis(timeZone.getRawOffset()).toHours();
			obj.putIfNotNull("timeZone", offset);
		}
		return varName -> "var " + varName + " = new google.visualization.DateFormat(" + obj.js() + ");";
	}
	
	public static Format DateFormat(final String pattern)
	{
		return DateFormat(pattern, null);
	}
	
	public static Format DateFormat(final String pattern, final TimeZone timeZone)
	{
		final ObjectHelper obj = new ObjectHelper().put("pattern", pattern);
		if(timeZone != null)
		{
			final int offset = (int)Duration.ofMillis(timeZone.getRawOffset()).toHours();
			obj.putIfNotNull("timeZone", offset);
		}
		return varName -> "var " + varName + " = new google.visualization.DateFormat(" + obj.js() + ");";
	}

	public static NumberFormatBuilder NumberFormatBuilder()
	{
		return new NumberFormatBuilder.Default();
	}
	
	public static interface NumberFormatBuilder
	{
		public NumberFormatBuilder decimalSymbol(Character decimalSymbol);

		public NumberFormatBuilder fractionDigits(Integer fractionDigits);

		public NumberFormatBuilder groupingSymbol(Character groupingSymbol);

		public NumberFormatBuilder negativeColor(String negativeColor);

		public NumberFormatBuilder negativeParens(Boolean negativeParens);

		public NumberFormatBuilder pattern(String pattern);

		public NumberFormatBuilder prefix(String prefix);

		public NumberFormatBuilder suffix(String suffix);
		
		public Format build();
		
		public static class Default implements NumberFormatBuilder
		{
			private Character decimalSymbol;
			private Integer   fractionDigits;
			private Character groupingSymbol;
			private String    negativeColor;
			private Boolean   negativeParens;
			private String    pattern;
			private String    prefix;
			private String    suffix;
			
			Default()
			{
				super();
			}
			
			@Override
			public NumberFormatBuilder decimalSymbol(final Character decimalSymbol)
			{
				this.decimalSymbol = decimalSymbol;
				return this;
			}

			@Override
			public NumberFormatBuilder fractionDigits(final Integer fractionDigits)
			{
				this.fractionDigits = fractionDigits;
				return this;
			}

			@Override
			public NumberFormatBuilder groupingSymbol(final Character groupingSymbol)
			{
				this.groupingSymbol = groupingSymbol;
				return this;
			}

			@Override
			public NumberFormatBuilder negativeColor(final String negativeColor)
			{
				this.negativeColor = negativeColor;
				return this;
			}

			@Override
			public NumberFormatBuilder negativeParens(final Boolean negativeParens)
			{
				this.negativeParens = negativeParens;
				return this;
			}

			@Override
			public NumberFormatBuilder pattern(final String pattern)
			{
				this.pattern = pattern;
				return this;
			}

			@Override
			public NumberFormatBuilder prefix(final String prefix)
			{
				this.prefix = prefix;
				return this;
			}

			@Override
			public NumberFormatBuilder suffix(final String suffix)
			{
				this.suffix = suffix;
				return this;
			}
			
			@Override
			public Format build()
			{
				return varName -> {
					final ObjectHelper obj = new ObjectHelper();
					obj.putIfNotNull("decimalSymbol", this.decimalSymbol);
					obj.putIfNotNull("fractionDigits", this.fractionDigits);
					obj.putIfNotNull("groupingSymbol", this.groupingSymbol);
					obj.putIfNotNull("negativeColor", this.negativeColor);
					obj.putIfNotNull("negativeParens", this.negativeParens);
					obj.putIfNotNull("pattern", this.pattern);
					obj.putIfNotNull("prefix", this.prefix);
					obj.putIfNotNull("suffix", this.suffix);
					return "var " + varName + " = new google.visualization.NumberFormat(" + obj.js() + ");";
				};
			}
		}
	}
	
	public static Format PatternFormat(final String pattern)
	{
		return varName -> "var " + varName + " = new google.visualization.PatternFormat(" + Static.js(pattern) + ");";
	}
}

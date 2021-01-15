/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import elemental.json.Json;
import elemental.json.JsonValue;


/**
 * @author XDEV Software
 * @since 10.02.00
 */
@FunctionalInterface
public interface JavaScriptable
{
	public String js();

	public static class Static
	{
		public static Object validateValue(final Object value)
		{
			if(value == null
				|| value instanceof JavaScriptable
				|| value instanceof JsonValue
				|| value instanceof CharSequence
				|| value instanceof Number
				|| value instanceof Boolean
				|| value instanceof Date
				|| value instanceof LocalDate
				|| value instanceof LocalTime
				|| value instanceof LocalDateTime)
			{
				return value;
			}

			throw new IllegalArgumentException("Unsupported value type: " + value.getClass().getName());
		}

		public static String js(final Object value)
		{
			if(value == null)
			{
				return "null";
			}
			if(value instanceof JavaScriptable)
			{
				return ((JavaScriptable)value).js();
			}
			if(value instanceof JsonValue)
			{
				return ((JsonValue)value).toJson();
			}
			if(value instanceof CharSequence)
			{
				return Static.js(value.toString());
			}
			if(value instanceof Character)
			{
				return Static.js((Character)value);
			}
			if(value instanceof Number)
			{
				return Static.js((Number)value);
			}
			if(value instanceof Boolean)
			{
				return Static.js((Boolean)value);
			}
			if(value instanceof Date)
			{
				return Static.js((Date)value);
			}
			if(value instanceof LocalDate)
			{
				return Static.js((LocalDate)value);
			}
			if(value instanceof LocalTime)
			{
				return Static.js((LocalTime)value);
			}
			if(value instanceof LocalDateTime)
			{
				return Static.js((LocalDateTime)value);
			}

			throw new IllegalArgumentException(String.valueOf(value));
		}

		public static String js(final String value)
		{
			return value != null
				? Json.create(value).toJson()
				: "null";
		}

		public static String js(final Character value)
		{
			return value != null
				? Json.create(value.toString()).toJson()
				: "null";
		}

		public static String js(final Number value)
		{
			return value != null
				? Json.create(value.doubleValue()).toJson()
				: "null";
		}

		public static String js(final Boolean value)
		{
			return value != null
				? Json.create(value).toJson()
				: "null";
		}

		public static String js(final JavaScriptable value)
		{
			return value != null
				? value.js()
				: "null";
		}

		public static String js(final Date value)
		{
			if(value == null)
			{
				return "null";
			}

			@SuppressWarnings("deprecation")
			final int year  = value.getYear();
			@SuppressWarnings("deprecation")
			final int month = value.getMonth();
			@SuppressWarnings("deprecation")
			final int day   = value.getDay();
			return "new Date(" + year + "," + month + "," + day + ")";
		}

		public static String js(final LocalDate value)
		{
			if(value == null)
			{
				return "null";
			}

			final int year  = value.getYear();
			final int month = value.getMonthValue() - 1;
			final int day   = value.getDayOfMonth();
			return "new Date(" + year + "," + month + "," + day + ")";
		}

		public static String js(final LocalTime value)
		{
			if(value == null)
			{
				return "null";
			}

			final int hour   = value.getHour();
			final int minute = value.getMinute();
			final int second = value.getSecond();
			return "new Date(0,0,0," + hour + "," + minute + "," + second + ")";
		}

		public static String js(final LocalDateTime value)
		{
			if(value == null)
			{
				return "null";
			}

			final int year   = value.getYear();
			final int month  = value.getMonthValue() - 1;
			final int day    = value.getDayOfMonth();
			final int hour   = value.getHour();
			final int minute = value.getMinute();
			final int second = value.getSecond();
			return "new Date(" + year + "," + month + "," + day + "," + hour + "," + minute + "," + second + ")";
		}

		private Static()
		{
			throw new Error();
		}
	}

	public static class ObjectHelper implements JavaScriptable
	{
		private final LinkedHashMap<String, String> values = new LinkedHashMap<>();

		public ObjectHelper()
		{
			super();
		}

		public ObjectHelper put(final String key, final Object value)
		{
			if(value instanceof JavaScriptable)
			{
				put(key, (JavaScriptable)value);
			}
			else if(value instanceof JsonValue)
			{
				put(key, (JsonValue)value);
			}
			else
			{
				this.values.put(key, Static.js(value));
			}

			return this;
		}

		public ObjectHelper put(final String key, final String value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final Character value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final Boolean value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final Number value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final Date value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final LocalDate value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final LocalTime value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final LocalDateTime value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final JavaScriptable value)
		{
			this.values.put(key, Static.js(value));
			return this;
		}

		public ObjectHelper put(final String key, final JsonValue value)
		{
			this.values.put(key, value != null ? value.toJson() : "null");
			return this;
		}

		public ObjectHelper putJson(final String key, final String value)
		{
			this.values.put(key, value);
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final Object value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}

			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final String value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final Character value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final Boolean value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final Number value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final Date value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final LocalDate value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final LocalTime value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final LocalDateTime value)
		{
			if(value != null)
			{
				this.values.put(key, Static.js(value));
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final JavaScriptable value)
		{
			if(value != null)
			{
				final String js = value.js();
				if(!(StringUtils.isEmpty(js) || js.equals("[]") || js.equals("{}")))
				{
					this.values.put(key, js);
				}
			}
			return this;
		}

		public ObjectHelper putIfNotNull(final String key, final JsonValue value)
		{
			if(value != null)
			{
				this.values.put(key, value.toJson());
			}
			return this;
		}

		public boolean isEmpty()
		{
			return this.values.isEmpty();
		}

		@Override
		public String js()
		{
			return this.values.entrySet().stream()
				.map(kv -> kv.getKey() + ":" + kv.getValue())
				.collect(Collectors.joining(",", "{", "}"));
		}
	}

	public static class ArrayHelper implements JavaScriptable
	{
		private final List<String> values = new ArrayList<>();

		public ArrayHelper()
		{
			super();
		}

		public ArrayHelper addAll(final Iterable<?> list)
		{
			if(list != null)
			{
				list.forEach(this::add);
			}
			return this;
		}

		public ArrayHelper addAllScriptables(final Iterable<? extends JavaScriptable> list)
		{
			if(list != null)
			{
				list.forEach(this::add);
			}
			return this;
		}

		public ArrayHelper addAllStrings(final Iterable<String> list)
		{
			if(list != null)
			{
				list.forEach(this::add);
			}
			return this;
		}

		public <N extends Number> ArrayHelper addAllNumbers(final Iterable<N> list)
		{
			if(list != null)
			{
				list.forEach(this::add);
			}
			return this;
		}

		public ArrayHelper add(final Object value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final String value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final Character value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final Boolean value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final Number value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final Date value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final LocalDate value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final LocalTime value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final LocalDateTime value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final JavaScriptable value)
		{
			this.values.add(Static.js(value));
			return this;
		}

		public ArrayHelper add(final JsonValue value)
		{
			this.values.add(value != null ? value.toJson() : "null");
			return this;
		}

		public ArrayHelper addJson(final String value)
		{
			this.values.add(value);
			return this;
		}

		public ArrayHelper addIfNotNull(final String value)
		{
			if(value != null)
			{
				this.values.add(Static.js(value));
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final Character value)
		{
			if(value != null)
			{
				this.values.add(Static.js(value));
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final Boolean value)
		{
			if(value != null)
			{
				this.values.add(Static.js(value));
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final Number value)
		{
			if(value != null)
			{
				this.values.add(Static.js(value));
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final Date value)
		{
			if(value != null)
			{
				this.values.add(Static.js(value));
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final LocalDate value)
		{
			if(value != null)
			{
				this.values.add(Static.js(value));
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final LocalTime value)
		{
			if(value != null)
			{
				this.values.add(Static.js(value));
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final LocalDateTime value)
		{
			if(value != null)
			{
				this.values.add(Static.js(value));
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final JavaScriptable value)
		{
			if(value != null)
			{
				final String js = value.js();
				if(!(StringUtils.isEmpty(js) || js.equals("[]") || js.equals("{}")))
				{
					this.values.add(js);
				}
			}
			return this;
		}

		public ArrayHelper addIfNotNull(final JsonValue value)
		{
			if(value != null)
			{
				this.values.add(value.toJson());
			}
			return this;
		}

		public boolean isEmpty()
		{
			return this.values.isEmpty();
		}

		@Override
		public String js()
		{
			return this.values.stream().collect(Collectors.joining(",", "[", "]"));
		}
	}
}

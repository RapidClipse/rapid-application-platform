/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.annotation;

import java.io.Serializable;
import java.util.Map;
import java.util.Objects;

import com.rapidclipse.framework.server.charts.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface NumberFormats extends Serializable, JavaScriptable
{
	public static NumberFormats ForAll(final String format)
	{
		return new ForAll(format);
	}

	public static NumberFormats SeriesBased(final Map<Integer, String> formats)
	{
		return new SeriesBased(formats);
	}
	
	public static class ForAll implements NumberFormats
	{
		private final String format;

		ForAll(final String format)
		{
			super();

			this.format = Objects.requireNonNull(format);
		}

		@Override
		public String js()
		{
			return Json.create(this.format).toJson();
		}
	}

	public static class SeriesBased implements NumberFormats
	{
		private final Map<Integer, String> formats;

		SeriesBased(final Map<Integer, String> formats)
		{
			super();

			this.formats = Objects.requireNonNull(formats);
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			this.formats.entrySet().forEach(e -> obj.put(e.getKey().toString(), e.getValue()));
			return obj.js();
		}
	}
}

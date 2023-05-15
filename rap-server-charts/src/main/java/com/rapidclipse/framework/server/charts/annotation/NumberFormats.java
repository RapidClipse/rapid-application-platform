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

/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

import java.io.Serializable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Interval extends Serializable, JavaScriptable
{
	public static enum Style implements JavaScriptable
	{
		LINE("line"),
		BAR("bar"),
		BOX("box"),
		STICK("stick"),
		POINT("point"),
		AREA("area");

		private final String js;

		private Style(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public Style style();

	public String color();

	public Number lineWidth();

	public Number barWidth();

	public Number boxWidth();

	public Number pointSize();

	public Number fillOpacity();

	public CurveType curveType();

	public static Interval New(final Style style)
	{
		return Builder().style(style).build();
	}
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder style(Style style);

		public Builder color(String color);

		public Builder lineWidth(Number lineWidth);

		public Builder barWidth(Number barWidth);

		public Builder boxWidth(Number boxWidth);

		public Builder pointSize(Number pointSize);

		public Builder fillOpacity(Number fillOpacity);

		public Builder curveType(CurveType curveType);

		public Interval build();
		
		public static class Default implements Builder
		{
			private Style     style;
			private String    color;
			private Number    lineWidth;
			private Number    barWidth;
			private Number    boxWidth;
			private Number    pointSize;
			private Number    fillOpacity;
			private CurveType curveType;
			
			Default()
			{
				super();
			}

			@Override
			public Builder style(final Style style)
			{
				this.style = style;
				return this;
			}

			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}

			@Override
			public Builder lineWidth(final Number lineWidth)
			{
				this.lineWidth = lineWidth;
				return this;
			}

			@Override
			public Builder barWidth(final Number barWidth)
			{
				this.barWidth = barWidth;
				return this;
			}

			@Override
			public Builder boxWidth(final Number boxWidth)
			{
				this.boxWidth = boxWidth;
				return this;
			}

			@Override
			public Builder pointSize(final Number pointSize)
			{
				this.pointSize = pointSize;
				return this;
			}

			@Override
			public Builder fillOpacity(final Number fillOpacity)
			{
				this.fillOpacity = fillOpacity;
				return this;
			}

			@Override
			public Builder curveType(final CurveType curveType)
			{
				this.curveType = curveType;
				return this;
			}

			@Override
			public Interval build()
			{
				return new Interval.Default(this.style, this.color, this.lineWidth, this.barWidth, this.boxWidth,
					this.pointSize, this.fillOpacity, this.curveType);
			}

		}

	}

	public static class Default implements Interval
	{
		private final Style     style;
		private final String    color;
		private final Number    lineWidth;
		private final Number    barWidth;
		private final Number    boxWidth;
		private final Number    pointSize;
		private final Number    fillOpacity;
		private final CurveType curveType;

		Default(
			final Style style,
			final String color,
			final Number lineWidth,
			final Number barWidth,
			final Number boxWidth,
			final Number pointSize,
			final Number fillOpacity,
			final CurveType curveType)
		{
			super();

			this.style       = style;
			this.color       = color;
			this.lineWidth   = lineWidth;
			this.barWidth    = barWidth;
			this.boxWidth    = boxWidth;
			this.pointSize   = pointSize;
			this.fillOpacity = fillOpacity;
			this.curveType   = curveType;
		}
		
		@Override
		public Style style()
		{
			return this.style;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Number lineWidth()
		{
			return this.lineWidth;
		}

		@Override
		public Number barWidth()
		{
			return this.barWidth;
		}

		@Override
		public Number boxWidth()
		{
			return this.boxWidth;
		}

		@Override
		public Number pointSize()
		{
			return this.pointSize;
		}

		@Override
		public Number fillOpacity()
		{
			return this.fillOpacity;
		}

		@Override
		public CurveType curveType()
		{
			return this.curveType;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("style", this.style);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("lineWidth", this.lineWidth);
			obj.putIfNotNull("barWidth", this.barWidth);
			obj.putIfNotNull("boxWidth", this.boxWidth);
			obj.putIfNotNull("pointSize", this.pointSize);
			obj.putIfNotNull("fillOpacity", this.fillOpacity);
			obj.putIfNotNull("curveType", this.curveType);
			return obj.js();
		}

	}

}

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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
public interface Trendline extends Serializable, JavaScriptable
{
	public static enum Type implements JavaScriptable
	{
		LINEAR("linear"),
		EXPONENTIAL("exponential"),
		POLYNOMIAL("polynomial");

		private final String js;

		private Type(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public String color();

	public Number degree();

	public String labelInLegend();

	public Number lineWidth();

	public Number opacity();

	public Number pointSize();

	public Boolean pointsVisible();

	public Boolean showR2();

	public Type type();

	public Boolean visibleInLegend();

	public static Trendline New()
	{
		return Builder().build();
	}
	
	public static Trendline New(final Type type)
	{
		return Builder().type(type).build();
	}

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder color(String color);

		public Builder degree(Number degree);

		public Builder labelInLegend(String labelInLegend);

		public Builder lineWidth(Number lineWidth);

		public Builder opacity(Number opacity);

		public Builder pointSize(Number pointSize);

		public Builder pointsVisible(Boolean pointsVisible);

		public Builder showR2(Boolean showR2);

		public Builder type(Type type);

		public Builder visibleInLegend(Boolean visibleInLegend);

		public Trendline build();

		public static class Default implements Builder
		{
			private String  color;
			private Number  degree;
			private String  labelInLegend;
			private Number  lineWidth;
			private Number  opacity;
			private Number  pointSize;
			private Boolean pointsVisible;
			private Boolean showR2;
			private Type    type;
			private Boolean visibleInLegend;
			
			Default()
			{
				super();
			}

			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}

			@Override
			public Builder degree(final Number degree)
			{
				this.degree = degree;
				return this;
			}

			@Override
			public Builder labelInLegend(final String labelInLegend)
			{
				this.labelInLegend = labelInLegend;
				return this;
			}

			@Override
			public Builder lineWidth(final Number lineWidth)
			{
				this.lineWidth = lineWidth;
				return this;
			}

			@Override
			public Builder opacity(final Number opacity)
			{
				this.opacity = opacity;
				return this;
			}

			@Override
			public Builder pointSize(final Number pointSize)
			{
				this.pointSize = pointSize;
				return this;
			}

			@Override
			public Builder pointsVisible(final Boolean pointsVisible)
			{
				this.pointsVisible = pointsVisible;
				return this;
			}

			@Override
			public Builder showR2(final Boolean showR2)
			{
				this.showR2 = showR2;
				return this;
			}

			@Override
			public Builder type(final Type type)
			{
				this.type = type;
				return this;
			}

			@Override
			public Builder visibleInLegend(final Boolean visibleInLegend)
			{
				this.visibleInLegend = visibleInLegend;
				return this;
			}

			@Override
			public Trendline build()
			{
				return new Trendline.Default(this.color, this.degree, this.labelInLegend, this.lineWidth, this.opacity,
					this.pointSize, this.pointsVisible,
					this.showR2, this.type, this.visibleInLegend);
			}
			
		}
		
	}

	public static class Default implements Trendline
	{
		private final String  color;
		private final Number  degree;
		private final String  labelInLegend;
		private final Number  lineWidth;
		private final Number  opacity;
		private final Number  pointSize;
		private final Boolean pointsVisible;
		private final Boolean showR2;
		private final Type    type;
		private final Boolean visibleInLegend;

		Default(
			final String color,
			final Number degree,
			final String labelInLegend,
			final Number lineWidth,
			final Number opacity,
			final Number pointSize,
			final Boolean pointsVisible,
			final Boolean showR2,
			final Type type,
			final Boolean visibleInLegend)
		{
			super();

			this.color           = color;
			this.degree          = degree;
			this.labelInLegend   = labelInLegend;
			this.lineWidth       = lineWidth;
			this.opacity         = opacity;
			this.pointSize       = pointSize;
			this.pointsVisible   = pointsVisible;
			this.showR2          = showR2;
			this.type            = type;
			this.visibleInLegend = visibleInLegend;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Number degree()
		{
			return this.degree;
		}

		@Override
		public String labelInLegend()
		{
			return this.labelInLegend;
		}

		@Override
		public Number lineWidth()
		{
			return this.lineWidth;
		}

		@Override
		public Number opacity()
		{
			return this.opacity;
		}

		@Override
		public Number pointSize()
		{
			return this.pointSize;
		}

		@Override
		public Boolean pointsVisible()
		{
			return this.pointsVisible;
		}

		@Override
		public Boolean showR2()
		{
			return this.showR2;
		}

		@Override
		public Type type()
		{
			return this.type;
		}

		@Override
		public Boolean visibleInLegend()
		{
			return this.visibleInLegend;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("degree", this.degree);
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("lineWidth", this.lineWidth);
			obj.putIfNotNull("opacity", this.opacity);
			obj.putIfNotNull("pointSize", this.pointSize);
			obj.putIfNotNull("pointsVisible", this.pointsVisible);
			obj.putIfNotNull("showR2", this.showR2);
			obj.putIfNotNull("type", this.type);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}
		
	}
	
}

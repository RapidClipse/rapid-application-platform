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
package com.rapidclipse.framework.server.charts.gantt;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Arrow extends Serializable, JavaScriptable
{
	public Number angle();

	public String color();

	public Number length();

	public Number radius();

	public Number spaceAfter();

	public Number width();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder angle(Number angle);

		public Builder color(String color);

		public Builder length(Number length);

		public Builder radius(Number radius);

		public Builder spaceAfter(Number spaceAfter);

		public Builder width(Number width);

		public Arrow build();
		
		public static class Default implements Builder
		{
			private Number angle;
			private String color;
			private Number length;
			private Number radius;
			private Number spaceAfter;
			private Number width;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder angle(final Number angle)
			{
				this.angle = angle;
				return this;
			}

			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}

			@Override
			public Builder length(final Number length)
			{
				this.length = length;
				return this;
			}

			@Override
			public Builder radius(final Number radius)
			{
				this.radius = radius;
				return this;
			}

			@Override
			public Builder spaceAfter(final Number spaceAfter)
			{
				this.spaceAfter = spaceAfter;
				return this;
			}

			@Override
			public Builder width(final Number width)
			{
				this.width = width;
				return this;
			}

			@Override
			public Arrow build()
			{
				return new Arrow.Default(this.angle, this.color, this.length, this.radius, this.spaceAfter, this.width);
			}
		}
	}

	public static class Default implements Arrow
	{
		private final Number angle;
		private final String color;
		private final Number length;
		private final Number radius;
		private final Number spaceAfter;
		private final Number width;

		Default(
			final Number angle,
			final String color,
			final Number length,
			final Number radius,
			final Number spaceAfter,
			final Number width)
		{
			super();
			
			this.angle      = angle;
			this.color      = color;
			this.length     = length;
			this.radius     = radius;
			this.spaceAfter = spaceAfter;
			this.width      = width;
		}

		@Override
		public Number angle()
		{
			return this.angle;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Number length()
		{
			return this.length;
		}

		@Override
		public Number radius()
		{
			return this.radius;
		}

		@Override
		public Number spaceAfter()
		{
			return this.spaceAfter;
		}

		@Override
		public Number width()
		{
			return this.width;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("angle", this.angle);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("length", this.length);
			obj.putIfNotNull("radius", this.radius);
			obj.putIfNotNull("spaceAfter", this.spaceAfter);
			obj.putIfNotNull("width", this.width);
			return obj.js();
		}
	}
}

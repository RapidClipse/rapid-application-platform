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

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface BoxStyle extends Serializable, JavaScriptable
{
	public String stroke();

	public Number strokeWidth();

	public Number radiusX();

	public Number radiusY();

	public Gradient gradient();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder stroke(String stroke);

		public Builder strokeWidth(Number strokeWidth);

		public Builder radiusX(Number radiusX);

		public Builder radiusY(Number radiusY);

		public Builder gradient(Gradient gradient);

		public BoxStyle build();

		public static class Default implements Builder
		{
			private String   stroke;
			private Number   strokeWidth;
			private Number   radiusX;
			private Number   radiusY;
			private Gradient gradient;

			Default()
			{
				super();
			}

			@Override
			public Builder stroke(final String stroke)
			{
				this.stroke = stroke;
				return this;
			}

			@Override
			public Builder strokeWidth(final Number strokeWidth)
			{
				this.strokeWidth = strokeWidth;
				return this;
			}

			@Override
			public Builder radiusX(final Number radiusX)
			{
				this.radiusX = radiusX;
				return this;
			}

			@Override
			public Builder radiusY(final Number radiusY)
			{
				this.radiusY = radiusY;
				return this;
			}

			@Override
			public Builder gradient(final Gradient gradient)
			{
				this.gradient = gradient;
				return this;
			}

			@Override
			public BoxStyle build()
			{
				return new BoxStyle.Default(this.stroke, this.strokeWidth, this.radiusX, this.radiusY, this.gradient);
			}
		}
	}

	public static BoxStyle New(
		final String stroke,
		final Number strokeWidth)
	{
		return new Default(stroke, strokeWidth, null, null, null);
	}

	public static BoxStyle New(
		final String stroke,
		final Number strokeWidth,
		final Number radiusX,
		final Number radiusY)
	{
		return new Default(stroke, strokeWidth, radiusX, radiusY, null);
	}

	public static BoxStyle New(
		final String stroke,
		final Number strokeWidth,
		final Number radiusX,
		final Number radiusY,
		final Gradient gradient)
	{
		return new Default(stroke, strokeWidth, radiusX, radiusY, gradient);
	}

	public static class Default implements BoxStyle
	{
		private final String   stroke;
		private final Number   strokeWidth;
		private final Number   radiusX;
		private final Number   radiusY;
		private final Gradient gradient;

		Default(
			final String stroke,
			final Number strokeWidth,
			final Number radiusX,
			final Number radiusY,
			final Gradient gradient)
		{
			super();

			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
			this.radiusX     = radiusX;
			this.radiusY     = radiusY;
			this.gradient    = gradient;
		}

		@Override
		public String stroke()
		{
			return this.stroke;
		}

		@Override
		public Number strokeWidth()
		{
			return this.strokeWidth;
		}

		@Override
		public Number radiusX()
		{
			return this.radiusX;
		}

		@Override
		public Number radiusY()
		{
			return this.radiusY;
		}

		@Override
		public Gradient gradient()
		{
			return this.gradient;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("rx", this.radiusX);
			obj.putIfNotNull("ry", this.radiusY);
			obj.putIfNotNull("gradient", this.gradient);
			return obj.js();
		}
	}
}

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.calendar;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface CellColor extends Serializable, JavaScriptable
{
	public String stroke();

	public Number strokeWidth();

	public Number strokeOpacity();

	public static Default New(final String stroke)
	{
		return new Default(stroke, null, null);
	}

	public static Default New(final String stroke, final Number strokeWidth)
	{
		return new Default(stroke, strokeWidth, null);
	}

	public static Default New(final String stroke, final Number strokeWidth, final Number strokeOpacity)
	{
		return new Default(stroke, strokeWidth, strokeOpacity);
	}

	public static class Default implements CellColor
	{
		private final String stroke;
		private final Number strokeWidth;
		private final Number strokeOpacity;

		Default(final String stroke, final Number strokeWidth, final Number strokeOpacity)
		{
			super();

			this.stroke        = stroke;
			this.strokeWidth   = strokeWidth;
			this.strokeOpacity = strokeOpacity;
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
		public Number strokeOpacity()
		{
			return this.strokeOpacity;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("strokeOpacity", this.strokeOpacity);
			return obj.js();
		}

	}

}

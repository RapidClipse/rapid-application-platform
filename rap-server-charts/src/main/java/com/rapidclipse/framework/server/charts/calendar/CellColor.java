/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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

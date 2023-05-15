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
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface CandlestickColor extends Serializable, JavaScriptable
{
	public String fill();

	public String stroke();

	public Number strokeWidth();

	public static Default New(final String fill)
	{
		return new Default(fill, null, null);
	}

	public static Default New(final String fill, final String stroke)
	{
		return new Default(fill, stroke, null);
	}

	public static Default New(final String fill, final String stroke, final Number strokeWidth)
	{
		return new Default(fill, stroke, strokeWidth);
	}

	public static class Default implements CandlestickColor
	{
		private final String fill;
		private final String stroke;
		private final Number strokeWidth;

		Default(final String fill, final String stroke, final Number strokeWidth)
		{
			super();

			this.fill        = fill;
			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
		}

		@Override
		public String fill()
		{
			return this.fill;
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
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("fill", this.fill);
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			return obj.js();
		}

	}

}

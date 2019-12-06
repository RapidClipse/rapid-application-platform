/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Objects;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Background extends Serializable, JavaScriptable
{
	public static Color Color(final String color)
	{
		return new Color(color);
	}
	
	public static class Color implements Background
	{
		private final String color;
		
		Color(final String color)
		{
			super();
			
			this.color = Objects.requireNonNull(color);
		}
		
		public String color()
		{
			return this.color;
		}
		
		@Override
		public String js()
		{
			return Json.create(this.color).toJson();
		}

	}
	
	public static StrokeFill StrokeFill(final String stroke, final Number strokeWidth, final String fill)
	{
		return new StrokeFill(stroke, strokeWidth, fill);
	}
	
	public static class StrokeFill implements Background
	{
		private final String stroke;
		private final Number strokeWidth;
		private final String fill;
		
		StrokeFill(final String stroke, final Number strokeWidth, final String fill)
		{
			super();
			
			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
			this.fill        = fill;
		}
		
		public String stroke()
		{
			return this.stroke;
		}
		
		public Number strokeWidth()
		{
			return this.strokeWidth;
		}
		
		public String fill()
		{
			return this.fill;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("fill", this.fill);
			return obj.js();
		}

	}

}

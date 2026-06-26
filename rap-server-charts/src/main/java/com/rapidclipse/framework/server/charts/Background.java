/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Objects;

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

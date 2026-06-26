/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.sankey;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface LinkColor extends Serializable, JavaScriptable
{
	public String stroke();
	
	public Number strokeWidth();
	
	public String fill();
	
	public Number fillOpacity();
	
	public static LinkColor
		New(final String stroke, final Number strokeWidth, final String fill, final Number fillOpacity)
	{
		return new Default(stroke, strokeWidth, fill, fillOpacity);
	}
	
	public static class Default implements LinkColor
	{
		private final String stroke;
		private final Number strokeWidth;
		private final String fill;
		private final Number fillOpacity;
		
		Default(final String stroke, final Number strokeWidth, final String fill, final Number fillOpacity)
		{
			super();
			
			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
			this.fill        = fill;
			this.fillOpacity = fillOpacity;
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
		public String fill()
		{
			return this.fill;
		}
		
		@Override
		public Number fillOpacity()
		{
			return this.fillOpacity;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("fill", this.fill);
			obj.putIfNotNull("fillOpacity", this.fillOpacity);
			return obj.js();
		}
		
	}
	
}

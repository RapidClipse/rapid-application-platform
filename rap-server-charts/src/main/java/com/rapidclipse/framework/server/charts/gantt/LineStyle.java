/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.gantt;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface LineStyle extends Serializable, JavaScriptable
{
	public String stroke();
	
	public Number strokeWidth();
	
	public static LineStyle New(final String stroke, final Number strokeWidth)
	{
		return new Default(stroke, strokeWidth);
	}
	
	public static class Default implements LineStyle
	{
		private final String stroke;
		private final Number strokeWidth;
		
		Default(final String stroke, final Number strokeWidth)
		{
			super();
			
			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
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
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			return obj.js();
		}
		
	}
	
}

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.bubble;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;
import com.rapidclipse.framework.server.charts.TextStyle;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Bubble extends Serializable, JavaScriptable
{
	public Number opacity();
	
	public String stroke();
	
	public TextStyle textStyle();
	
	public static Bubble New(final String stroke)
	{
		return new Default(null, stroke, null);
	}
	
	public static Bubble New(final Number opacity, final String stroke)
	{
		return new Default(opacity, stroke, null);
	}
	
	public static Bubble New(final String stroke, final TextStyle textStyle)
	{
		return new Default(null, stroke, textStyle);
	}
	
	public static Bubble New(final Number opacity, final TextStyle textStyle)
	{
		return new Default(opacity, null, textStyle);
	}
	
	public static Bubble New(final Number opacity, final String stroke, final TextStyle textStyle)
	{
		return new Default(opacity, stroke, textStyle);
	}
	
	public static class Default implements Bubble
	{
		private final Number    opacity;
		private final String    stroke;
		private final TextStyle textStyle;
		
		Default(final Number opacity, final String stroke, final TextStyle textStyle)
		{
			super();
			
			this.opacity   = opacity;
			this.stroke    = stroke;
			this.textStyle = textStyle;
		}
		
		@Override
		public Number opacity()
		{
			return this.opacity;
		}
		
		@Override
		public String stroke()
		{
			return this.stroke;
		}
		
		@Override
		public TextStyle textStyle()
		{
			return this.textStyle;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("opacity", this.opacity);
			obj.putIfNotNull("stroke", this.opacity);
			obj.putIfNotNull("textStyle", this.textStyle);
			return obj.js();
		}

	}

}

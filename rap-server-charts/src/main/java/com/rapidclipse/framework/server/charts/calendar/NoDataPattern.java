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
public interface NoDataPattern extends Serializable, JavaScriptable
{
	public String backgroundColor();
	
	public String color();
	
	public static NoDataPattern New(final String backgroundColor, final String color)
	{
		return new Default(backgroundColor, color);
	}
	
	public static class Default implements NoDataPattern
	{
		private final String backgroundColor;
		private final String color;
		
		Default(final String backgroundColor, final String color)
		{
			super();
			
			this.backgroundColor = backgroundColor;
			this.color           = color;
		}
		
		@Override
		public String backgroundColor()
		{
			return this.backgroundColor;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("backgroundColor", this.backgroundColor);
			obj.putIfNotNull("color", this.color);
			return obj.js();
		}
		
	}
	
}

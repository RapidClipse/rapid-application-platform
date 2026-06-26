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


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface GridLines extends Serializable, JavaScriptable
{
	public String color();
	
	public Integer count();
	
	public static GridLines New(final String color)
	{
		return new Default(color, null);
	}
	
	public static GridLines New(final Integer count)
	{
		return new Default(null, count);
	}
	
	public static GridLines New(final String color, final Integer count)
	{
		return new Default(color, count);
	}
	
	public static class Default implements GridLines
	{
		private final String  color;
		private final Integer count;
		
		Default(final String color, final Integer count)
		{
			super();
			
			this.color = color;
			this.count = count;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public Integer count()
		{
			return this.count;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("count", this.count);
			return obj.js();
		}
		
	}
	
}

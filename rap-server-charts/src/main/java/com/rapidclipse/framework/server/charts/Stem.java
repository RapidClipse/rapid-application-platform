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
public interface Stem extends Serializable, JavaScriptable
{
	public String color();
	
	public Number length();
	
	public static Stem New(final String color)
	{
		return new Default(color, null);
	}

	public static Stem New(final Number length)
	{
		return new Default(null, length);
	}

	public static Stem New(final String color, final Number length)
	{
		return new Default(color, length);
	}
	
	public static class Default implements Stem
	{
		private final String color;
		private final Number length;
		
		Default(final String stemColor, final Number stemLength)
		{
			super();
			
			this.color  = stemColor;
			this.length = stemLength;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public Number length()
		{
			return this.length;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("length", this.length);
			return obj.js();
		}
	}
}

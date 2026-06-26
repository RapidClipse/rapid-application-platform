/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.maps;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;
import com.rapidclipse.framework.server.charts.TextStyle;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Legend extends Serializable, JavaScriptable
{
	public String numberFormat();

	public TextStyle textStyle();

	public static Legend New(final String numberFormat, final TextStyle textStyle)
	{
		return new Default(numberFormat, textStyle);
	}

	public static Legend New(final String numberFormat)
	{
		return new Default(numberFormat, null);
	}

	public static Legend New(final TextStyle textStyle)
	{
		return new Default(null, textStyle);
	}
	
	public static class Default implements Legend
	{
		private final String    numberFormat;
		private final TextStyle textStyle;

		Default(final String numberFormat, final TextStyle textStyle)
		{
			super();

			this.numberFormat = numberFormat;
			this.textStyle    = textStyle;
		}
		
		@Override
		public String numberFormat()
		{
			return this.numberFormat;
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
			obj.putIfNotNull("numberFormat", this.numberFormat);
			obj.putIfNotNull("textStyle", this.textStyle);
			return obj.js();
		}

	}

}

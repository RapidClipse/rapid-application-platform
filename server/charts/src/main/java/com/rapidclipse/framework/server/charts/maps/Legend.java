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

package com.rapidclipse.framework.server.charts.maps;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.util.JavaScriptable;


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

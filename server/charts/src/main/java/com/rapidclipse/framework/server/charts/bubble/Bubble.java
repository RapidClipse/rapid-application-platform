/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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

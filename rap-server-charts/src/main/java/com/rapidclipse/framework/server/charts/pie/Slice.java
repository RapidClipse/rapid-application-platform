/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.pie;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;
import com.rapidclipse.framework.server.charts.TextStyle;


/**
 * @author XDEV Software
 * @since 10.02.00
 */
public interface Slice extends Serializable, JavaScriptable
{
	public Number offset();
	
	public String color();
	
	public TextStyle textStyle();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder offset(Number offset);
		
		public Builder color(String color);
		
		public Builder textStyle(TextStyle textStyle);
		
		public Slice build();
		
		public static class Default implements Builder
		{
			private Number    offset;
			private String    color;
			private TextStyle textStyle;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder offset(final Number offset)
			{
				this.offset = offset;
				return this;
			}
			
			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}
			
			@Override
			public Builder textStyle(final TextStyle textStyle)
			{
				this.textStyle = textStyle;
				return this;
			}
			
			@Override
			public Slice build()
			{
				return Slice.New(this.offset, this.color, this.textStyle);
			}
			
		}
		
	}
	
	public static Slice New(final Number offset)
	{
		return new Default(offset, null, null);
	}
	
	public static Slice New(final String color)
	{
		return new Default(null, color, null);
	}
	
	public static Slice New(final TextStyle textStyle)
	{
		return new Default(null, null, textStyle);
	}
	
	public static Slice New(final Number offset, final String color, final TextStyle textStyle)
	{
		return new Default(offset, color, textStyle);
	}
	
	public static class Default implements Slice
	{
		private final Number    offset;
		private final String    color;
		private final TextStyle textStyle;
		
		Default(final Number offset, final String color, final TextStyle textStyle)
		{
			super();
			
			this.offset    = offset;
			this.color     = color;
			this.textStyle = textStyle;
		}
		
		@Override
		public Number offset()
		{
			return this.offset;
		}
		
		@Override
		public String color()
		{
			return this.color;
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
			obj.putIfNotNull("offset", this.offset);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("textStyle", this.textStyle);
			return obj.js();
		}
		
	}
	
}

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
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public interface TextStyle extends Serializable, JavaScriptable
{
	public String color();
	
	public String fontName();
	
	public Number fontSize();
	
	public Boolean bold();
	
	public Boolean italic();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder color(String color);
		
		public Builder fontName(String fontName);
		
		public Builder fontSize(Number fontSize);
		
		public Builder bold();
		
		public Builder italic();
		
		public TextStyle build();
		
		public static class Default implements Builder
		{
			private String  color;
			private String  fontName;
			private Number  fontSize;
			private Boolean bold;
			private Boolean italic;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}
			
			@Override
			public Builder fontName(final String fontName)
			{
				this.fontName = fontName;
				return this;
			}
			
			@Override
			public Builder fontSize(final Number fontSize)
			{
				this.fontSize = fontSize;
				return this;
			}
			
			@Override
			public Builder bold()
			{
				this.bold = Boolean.TRUE;
				return this;
			}
			
			@Override
			public Builder italic()
			{
				this.italic = Boolean.TRUE;
				return this;
			}
			
			@Override
			public TextStyle build()
			{
				return TextStyle.New(this.color, this.fontName, this.fontSize, this.bold, this.italic);
			}
			
		}
		
	}
	
	public static TextStyle New(
		final String color)
	{
		return new Default(color, null, null, null, null);
	}
	
	public static TextStyle New(
		final String color,
		final String fontName)
	{
		return new Default(color, fontName, null, null, null);
	}
	
	public static TextStyle New(
		final String color,
		final String fontName,
		final Number fontSize)
	{
		return new Default(color, fontName, fontSize, null, null);
	}
	
	public static TextStyle New(
		final String fontName,
		final Number fontSize)
	{
		return new Default(null, fontName, fontSize, null, null);
	}
	
	public static TextStyle New(
		final String color,
		final String fontName,
		final Number fontSize,
		final Boolean bold,
		final Boolean italic)
	{
		return new Default(color, fontName, fontSize, bold, italic);
	}
	
	public static class Default implements TextStyle
	{
		private final String  color;
		private final String  fontName;
		private final Number  fontSize;
		private final Boolean bold;
		private final Boolean italic;
		
		Default(
			final String color,
			final String fontName,
			final Number fontSize,
			final Boolean bold,
			final Boolean italic)
		{
			super();
			
			this.color    = color;
			this.fontName = fontName;
			this.fontSize = fontSize;
			this.bold     = bold;
			this.italic   = italic;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public String fontName()
		{
			return this.fontName;
		}
		
		@Override
		public Number fontSize()
		{
			return this.fontSize;
		}
		
		@Override
		public Boolean bold()
		{
			return this.bold;
		}
		
		@Override
		public Boolean italic()
		{
			return this.italic;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("fontName", this.fontName);
			obj.putIfNotNull("fontSize", this.fontSize);
			obj.putIfNotNull("bold", this.bold);
			obj.putIfNotNull("italic", this.italic);
			return obj.js();
		}
		
	}
	
}

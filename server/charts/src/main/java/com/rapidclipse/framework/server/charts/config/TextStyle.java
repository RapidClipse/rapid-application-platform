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
package com.rapidclipse.framework.server.charts.config;

/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class TextStyle
{
	
	private String  color  = "black";
	private String  fontName;
	private Integer fontSize;
	private Boolean bold   = false;
	private Boolean italic = false;
	
	public String getColor()
	{
		return this.color;
	}
	
	/**
	 * The color can be any HTML color string, for example: 'red' or '#00cc00' <br>
	 *
	 * @param color
	 */
	public void setColor(final String color)
	{
		this.color = color;
	}
	
	public String getFontName()
	{
		return this.fontName;
	}
	
	/**
	 * Sets the font name of the text style <br>
	 *
	 * @param fontName
	 */
	public void setFontName(final String fontName)
	{
		this.fontName = fontName;
	}
	
	public Integer getFontSize()
	{
		return this.fontSize;
	}
	
	/**
	 * The font size, in pixels <br>
	 *
	 * @param fontName
	 */
	public void setFontSize(final Integer fontSize)
	{
		this.fontSize = fontSize;
	}
	
	public Boolean getBold()
	{
		return this.bold;
	}
	
	public void setBold(final Boolean bold)
	{
		this.bold = bold;
	}
	
	public Boolean getItalic()
	{
		return this.italic;
	}
	
	public void setItalic(final Boolean italic)
	{
		this.italic = italic;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ color: '" + this.color + "', ");
		if(this.fontName != null)
		{
			str.append("fontName: '" + this.fontName + "', ");
		}
		if(this.fontSize != null)
		{
			str.append("fontSize: " + this.fontSize + ", ");
		}
		str.append("bold: " + this.bold + ", ");
		str.append("italic: " + this.italic + " }");

		return str.toString();
	}
}

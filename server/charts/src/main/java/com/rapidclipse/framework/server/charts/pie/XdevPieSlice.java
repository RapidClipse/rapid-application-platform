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
package com.rapidclipse.framework.server.charts.pie;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.config.TextStyle;


/**
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevPieSlice implements Serializable
{
	private String color;
	private Double offset = 0.0;
	
	private TextStyle textStyle = new TextStyle();
	
	/**
	 * @return the color
	 */
	public String getColor()
	{
		return this.color;
	}
	
	/**
	 * The color to use for this slice. Specify a valid HTML color string. <br>
	 *
	 * @param color
	 *            the color to set
	 */
	public void setColor(final String color)
	{
		this.color = color;
	}
	
	/**
	 * @return the offset
	 */
	public Double getOffset()
	{
		return this.offset;
	}
	
	/**
	 * How far to separate the slice from the rest of the pie, from 0.0 (not at all)
	 * to 1.0 (the pie's radius). <br>
	 *
	 * @param offset
	 *            the offset to set
	 */
	public void setOffset(final Double offset)
	{
		this.offset = offset;
	}
	
	/**
	 * @return the pieSliceTextStyle
	 */
	public TextStyle getTextStyle()
	{
		return this.textStyle;
	}
	
	/**
	 * An object that specifies the slice text style. <br>
	 *
	 * @param pieSliceTextStyle
	 *            the pieSliceTextStyle to set
	 */
	public void setTextStyle(final TextStyle textStyle)
	{
		this.textStyle = textStyle;
	}

	@Override
	public String toString()
	{
		return " {color: '" + this.color + "'}, " +
			"{offset: " + this.offset + "}, "
			+ this.textStyle + " ";
	}

}

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

import java.io.Serializable;
import java.util.List;

import com.rapidclipse.framework.server.charts.TextStyle;


/**
 * TODO common type for H and V axis
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class VAxis implements Serializable
{
	private String      title            = "";
	private List<Ticks> ticks;
	private TextStyle   titleTextStyle;
	private TextStyle   textStyle;
	private boolean     slantedText;
	private Integer     slantedTextAngle = 30;
	private String      textPosition     = "out";
	private Integer     maxValue;
	private Integer     minValue;
	
	public VAxis(final String title)
	{
		this.title = title;
	}
	
	public String getTitle()
	{
		return this.title;
	}
	
	/**
	 * Title property that specifies a title for the vertical axis. <br>
	 *
	 * @param title
	 */
	public void setTitle(final String title)
	{
		this.title = title;
	}
	
	public List<Ticks> getTicks()
	{
		return this.ticks;
	}
	
	/**
	 *
	 * Replaces the automatically generated X-axis ticks with the specified array.
	 * Each element of the array should be either a valid tick value (such as a
	 * number, date, datetime, or timeofday), or an object.
	 *
	 * @param ticks
	 */
	public void setTicks(final List<Ticks> ticks)
	{
		this.ticks = ticks;
	}
	
	public TextStyle getTextStyle()
	{
		return this.textStyle;
	}
	
	/**
	 * An object that specifies the vertical axis text style. <br>
	 *
	 * @param textStyle
	 */
	public void setTextStyle(final TextStyle textStyle)
	{
		this.textStyle = textStyle;
	}
	
	public boolean isSlantedText()
	{
		return this.slantedText;
	}
	
	/**
	 * If true, draw the vertical axis text at an angle, to help fit more text along
	 * the axis; if false, draw vertical axis text upright. <br>
	 *
	 * @param slantedText
	 */
	public void setSlantedText(final boolean slantedText)
	{
		this.slantedText = slantedText;
	}
	
	public Integer getSlantedTextAngle()
	{
		return this.slantedTextAngle;
	}
	
	/**
	 * The angle of the vertical axis text, if it's drawn slanted. Ignored if
	 * vertical.slantedText is false, or is in auto mode, and the chart decided to
	 * draw the text vertically. <br>
	 *
	 * @param slantedTextAngle
	 *            Integer 1—90
	 */
	public void setSlantedTextAngle(final Integer slantedTextAngle)
	{
		this.slantedTextAngle = slantedTextAngle;
	}
	
	public TextStyle getTitleTextStyle()
	{
		return this.titleTextStyle;
	}
	
	/**
	 * An object that specifies the vertical axis title text style. <br>
	 *
	 * @param titleTextStyle
	 */
	public void setTitleTextStyle(final TextStyle titleTextStyle)
	{
		this.titleTextStyle = titleTextStyle;
	}
	
	public String getTextPosition()
	{
		return this.textPosition;
	}
	
	/**
	 * Position of the vertical axis text, relative to the chart area. Supported
	 * values: 'out', 'in', 'none'. <br>
	 *
	 * @param textPosition
	 */
	public void setTextPosition(final String textPosition)
	{
		this.textPosition = textPosition;
	}
	
	public int getMaxValue()
	{
		return this.maxValue;
	}
	
	/**
	 * Moves the max value of the vertical axis to the specified value; this will be upward in most charts.
	 * Ignored if this is set to a value smaller than the maximum y-value of the data.
	 *
	 * @param maxValue
	 */
	public void setMaxValue(final int maxValue)
	{
		this.maxValue = maxValue;
	}
	
	public int getMinValue()
	{
		return this.minValue;
	}
	
	/**
	 * Moves the min value of the vertical axis to the specified value,
	 * this will be downward in most charts.
	 * Ignored if this is set to a value greater than the minimum y-value of the data.
	 *
	 * @param minValue
	 */
	public void setMinValue(final int minValue)
	{
		this.minValue = minValue;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		if(this.title != null)
		{
			str.append("title: '" + this.title + "', ");
		}
		if(this.titleTextStyle != null)
		{
			str.append("titleTextStyle: " + this.titleTextStyle + ", ");
		}
		if(this.textStyle != null)
		{
			str.append("textStyle: " + this.textStyle + ", ");
		}
		if(this.ticks != null)
		{
			str.append("ticks: " + this.ticks + ", ");
		}
		if(this.slantedText && this.textPosition.equals("out"))
		{
			str.append("slantedText: " + this.slantedText + ", ");
		}
		if(this.slantedText)
		{
			str.append("slantedTextAngle: " + this.slantedTextAngle + ", ");
		}

		str.append("maxValue: " + this.maxValue + ", ");
		str.append("minValue: " + this.minValue + ", ");

		str.append("textPosition: '" + this.textPosition + "' }");

		return str.toString();
	}
}

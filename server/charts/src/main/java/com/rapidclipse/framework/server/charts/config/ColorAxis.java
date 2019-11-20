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

import java.util.ArrayList;
import java.util.List;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public class ColorAxis
{
	private Integer      minValue;
	private Integer      maxValue;
	private List<String> colors = new ArrayList<>();
	
	/**
	 * @return the minValue
	 */
	public Integer getMinValue()
	{
		return this.minValue;
	}
	
	/**
	 * If present, specifies a minimum value for chart color data. Color data values
	 * of this value and lower will be rendered as the first color in the
	 * colorAxis.colors range. <br>
	 *
	 * @param minValue
	 *            the minValue to set
	 */
	public void setMinValue(final Integer minValue)
	{
		this.minValue = minValue;
	}
	
	/**
	 * @return the maxValue
	 */
	public Integer getMaxValue()
	{
		return this.maxValue;
	}
	
	/**
	 * If present, specifies a maximum value for chart color data. Color data values
	 * of this value and higher will be rendered as the last color in the
	 * colorAxis.colors range. <br>
	 *
	 * @param maxValue
	 *            the maxValue to set
	 */
	public void setMaxValue(final Integer maxValue)
	{
		this.maxValue = maxValue;
	}
	
	/**
	 * @return the colors
	 */
	public List<String> getColors()
	{
		return this.colors;
	}
	
	/**
	 * Colors to assign to values in the visualization. An array of strings, where
	 * each element is an HTML color string, for example: 'red', #004411. You must
	 * have at least two values; the gradient will include all your values, plus
	 * calculated intermediary values, with the first color as the smallest value,
	 * and the last color as the highest. <br>
	 *
	 * @param colors
	 *            the colors to set
	 */
	public void setColors(final List<String> colors)
	{
		final List<String> colorsString = new ArrayList<>();
		for(final String color : colors)
		{
			colorsString.add("'" + color + "'");
		}
		this.colors = colorsString;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		str.append("minValue: " + this.minValue + ", ");
		str.append("maxValue: " + this.maxValue + ", ");
		str.append("colors: " + this.colors);
		str.append("}");
		return str.toString();
	}

}

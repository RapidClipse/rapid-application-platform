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

package com.rapidclipse.framework.server.charts.line;

import java.util.List;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Intervals
{
	private String               style     = "line";
	private Integer              pointSize = 2;
	private double               lineWidth = 0.5;
	private double               barWidth  = 0.5;
	private String               color;
	private double               boxWidth  = 0.5;
	private final List<Interval> intervals;
	
	/**
	 * Set a List of Intervals (or null) for global settings of intervals
	 *
	 * @param intervals
	 */
	public Intervals(final List<Interval> intervals)
	{
		this.intervals = intervals;
	}

	public String getStyle()
	{
		return this.style;
	}

	/**
	 * 'line', 'bar', 'box', 'stick', 'point' and 'area'.
	 *
	 * @param style
	 */
	public void setStyle(final String style)
	{
		this.style = style;
	}

	public Integer getPointSize()
	{
		return this.pointSize;
	}

	public void setPointSize(final Integer pointSize)
	{
		this.pointSize = pointSize;
	}

	public double getLineWidth()
	{
		return this.lineWidth;
	}

	public void setLineWidth(final double lineWidth)
	{
		this.lineWidth = lineWidth;
	}

	public double getBarWidth()
	{
		return this.barWidth;
	}

	public void setBarWidth(final double barWidth)
	{
		this.barWidth = barWidth;
	}

	public String getColor()
	{
		return this.color;
	}

	public void setColor(final String color)
	{
		this.color = color;
	}

	public double getBoxWidth()
	{
		return this.boxWidth;
	}

	public void setBoxWidth(final double boxWidth)
	{
		this.boxWidth = boxWidth;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("boxWidth: " + this.boxWidth + ", ");
		str.append("pointSize: " + this.pointSize + ", ");
		str.append("lineWidth: " + this.lineWidth + ", ");
		str.append("barWidth: " + this.barWidth + ", ");
		if(this.color != null)
		{
			str.append("color: '" + this.color + "', ");
		}
		str.append("'style': '" + this.style + "' ");
		str.append("}");
		if(this.intervals != null)
		{
			str.append(", interval: { ");
			for(final Interval i : this.intervals)
			{
				str.append("'" + i.getName() + "': " + i + ",");
			}
			str.delete(str.length() - 1, str.length());
			str.append("}");
		}
		return str.toString();
	}

}

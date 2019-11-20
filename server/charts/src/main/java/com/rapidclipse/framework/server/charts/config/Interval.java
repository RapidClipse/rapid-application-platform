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
public class Interval
{
	private String  name;
	private String  style;
	private Integer fillOpacity;
	private String  color;
	private Integer pointSize;
	private Integer lineWidth;
	private String  curveType = "none";
	
	/**
	 * setting specific intervals for data rows
	 *
	 * @param name
	 *            caption name
	 */
	public Interval(final String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return this.name;
	}

	public void setName(final String name)
	{
		this.name = name;
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
	
	public Integer getFillOpacity()
	{
		return this.fillOpacity;
	}
	
	public void setFillOpacity(final Integer fillOpacity)
	{
		this.fillOpacity = fillOpacity;
	}
	
	public String getColor()
	{
		return this.color;
	}
	
	public void setColor(final String color)
	{
		this.color = color;
	}
	
	public Integer getPointSize()
	{
		return this.pointSize;
	}
	
	public void setPointSize(final Integer pointSize)
	{
		this.pointSize = pointSize;
	}
	
	public Integer getLineWidth()
	{
		return this.lineWidth;
	}
	
	public void setLineWidth(final Integer lineWidth)
	{
		this.lineWidth = lineWidth;
	}
	
	public String getCurveType()
	{
		return this.curveType;
	}
	
	/**
	 * <li>'function' The angles of the line will be smoothed.</li>
	 * <li>'none' Straight lines without curve.</li>
	 *
	 * @param curveType
	 */
	public void setCurveType(final String curveType)
	{
		this.curveType = curveType;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("fillOpacity: " + this.fillOpacity + ", ");
		str.append("pointSize: " + this.pointSize + ", ");
		str.append("lineWidth: " + this.lineWidth + ", ");
		str.append("curveType: '" + this.curveType + "', ");
		if(this.color != null)
		{
			str.append("color: '" + this.color + "', ");
		}
		str.append("style: '" + this.style + "' ");
		str.append("}");
		return str.toString();
	}
}

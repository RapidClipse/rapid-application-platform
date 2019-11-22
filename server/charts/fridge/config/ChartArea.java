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

import org.apache.commons.lang3.StringUtils;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class ChartArea
{
	private String backgroundColor = "white";
	private String left            = "auto";
	private String right           = "auto";
	private String top             = "auto";
	private String width           = "auto";
	private String height          = "auto";
	
	public String getBackgroundColor()
	{
		return this.backgroundColor;
	}
	
	/**
	 * Chart area background color. When a string is used, it can be either a hex
	 * string (e.g., '#fdc') or an English color name. <br>
	 *
	 * @param backgroundColor
	 */
	public void setBackgroundColor(final String backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}
	
	public String getLeft()
	{
		return this.left;
	}
	
	/**
	 * How far to draw the chart from the left border. <br>
	 * Left distance in pixel or percentage(0-100%)
	 *
	 * @param left
	 */
	public void setLeft(final String left)
	{
		this.left = left;
	}
	
	public String getRight()
	{
		return this.right;
	}
	
	/**
	 * How far to draw the chart from the right border. <br>
	 * Right distance in pixel or percentage(0-100%)
	 *
	 * @param right
	 */
	public void setRight(final String right)
	{
		this.right = right;
	}
	
	public String getTop()
	{
		return this.top;
	}
	
	/**
	 * How far to draw the chart from the top border. <br>
	 * Top distance in pixel or percentage(0-100%)
	 *
	 * @param Top
	 */
	public void setTop(final String top)
	{
		this.top = top;
	}
	
	public String getWidth()
	{
		return this.width;
	}
	
	public void setWidth(final String width)
	{
		this.width = width;
	}
	
	public String getHeight()
	{
		return this.height;
	}
	
	public void setHeight(final String heigth)
	{
		this.height = heigth;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		
		str.append("{ ");
		
		str.append("left: ");
		
		if(StringUtils.isNumeric(this.left))
		{
			str.append(this.left);
		}
		else
		{
			str.append("'" + this.left + "'");
		}
		
		str.append(", right: ");
		if(StringUtils.isNumeric(this.right))
		{
			str.append(this.right);
		}
		else
		{
			str.append("'" + this.right + "'");
		}
		
		str.append(", top: ");
		if(StringUtils.isNumeric(this.top))
		{
			str.append(this.top);
		}
		else
		{
			str.append("'" + this.top + "'");
		}
		
		str.append(", width: ");
		if(StringUtils.isNumeric(this.width))
		{
			str.append(this.width);
		}
		else
		{
			str.append("'" + this.width + "'");
		}
		
		str.append(", height: ");
		if(StringUtils.isNumeric(this.height))
		{
			str.append(this.height);
		}
		else
		{
			str.append("'" + this.height + "'");
		}
		
		str.append("}");
		return str.toString();
		
	}
	
}

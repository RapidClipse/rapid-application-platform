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
public class TimelineOptions
{
	private boolean   colorByRowLabel = false;
	private boolean   groupByRowLabel = true;
	private TextStyle rowLabelStyle;
	private boolean   showBarLabels   = true;
	private boolean   showRowLabels   = true;
	private String    singleColor;
	private TextStyle barLabelStyle;
	
	public TextStyle getBarLabelStyle()
	{
		return this.barLabelStyle;
	}
	
	/**
	 * An 'TextStyle' object that specifies the bar label text style. <br>
	 *
	 * @param barLabelStyle
	 */
	public void setBarLabelStyle(final TextStyle barLabelStyle)
	{
		this.barLabelStyle = barLabelStyle;
	}
	
	public boolean isColorByRowLabel()
	{
		return this.colorByRowLabel;
	}
	
	/**
	 * If set to true, colors every bar on the row the same. The default is to use
	 * one color per bar label. <br>
	 *
	 * @param colorByRowLabel
	 */
	public void setColorByRowLabel(final boolean colorByRowLabel)
	{
		this.colorByRowLabel = colorByRowLabel;
	}
	
	public boolean isGroupByRowLabel()
	{
		return this.groupByRowLabel;
	}
	
	/**
	 * If set to false, creates one row for every entry. The default is to collect
	 * bars with the same row label into one row. <br>
	 *
	 * @param groupByRowLabel
	 */
	public void setGroupByRowLabel(final boolean groupByRowLabel)
	{
		this.groupByRowLabel = groupByRowLabel;
	}
	
	public TextStyle getRowLabelStyle()
	{
		return this.rowLabelStyle;
	}
	
	/**
	 * An 'TextStyle' object that specifies the bar label text style. <br>
	 *
	 * @param rowLabelStyle
	 */
	public void setRowLabelStyle(final TextStyle rowLabelStyle)
	{
		this.rowLabelStyle = rowLabelStyle;
	}
	
	public boolean isShowBarLabels()
	{
		return this.showBarLabels;
	}
	
	/**
	 * If set to false, omits bar labels. The default is to show them. <br>
	 *
	 * @param showBarLabels
	 */
	public void setShowBarLabels(final boolean showBarLabels)
	{
		this.showBarLabels = showBarLabels;
	}
	
	public boolean isShowRowLabels()
	{
		return this.showRowLabels;
	}
	
	/**
	 * If set to false, omits row labels. The default is to show them. <br>
	 *
	 * @param showRowLabels
	 */
	public void setShowRowLabels(final boolean showRowLabels)
	{
		this.showRowLabels = showRowLabels;
	}
	
	public String getSingleColor()
	{
		return this.singleColor;
	}
	
	/**
	 * Colors all bars the same. Specified as a hex value (e.g., '#8d8'). <br>
	 *
	 * @param singleColor
	 */
	public void setSingleColor(final String singleColor)
	{
		this.singleColor = singleColor;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("colorByRowLabel: " + this.colorByRowLabel + ", ");
		str.append("groupByRowLabel: " + this.groupByRowLabel + ", ");
		str.append("rowLabelStyle: " + this.rowLabelStyle + ", ");
		str.append("showBarLabels: " + this.showBarLabels + ", ");
		str.append("showRowLabels: " + this.showRowLabels + ", ");
		if(this.singleColor != null)
		{
			str.append("singleColor: '" + this.singleColor + "', ");
		}
		str.append("barLabelStyle: " + this.barLabelStyle);
		str.append("}");

		return str.toString();
	}
}

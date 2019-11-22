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

package com.rapidclipse.framework.server.charts.calendar;

import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.charts.config.BackgroundStyle;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Calendar
{
	private BackgroundStyle cellColor;
	private Integer         cellSize = 16;
	private TextStyle       dayOfWeekLabel;
	private TextStyle       monthLabel;

	public BackgroundStyle getCellColor()
	{
		return this.cellColor;
	}

	/**
	 * customize the border of the calendar day squares
	 *
	 * @param cellColor
	 */
	public void setCellColor(final BackgroundStyle cellColor)
	{
		this.cellColor = cellColor;
	}
	
	public Integer getCellSize()
	{
		return this.cellSize;
	}
	
	/**
	 * The size of the calendar day squares
	 *
	 * @param cellSize
	 */
	public void setCellSize(final Integer cellSize)
	{
		this.cellSize = cellSize;
	}

	public TextStyle getDayOfWeekLabel()
	{
		return this.dayOfWeekLabel;
	}

	/**
	 * Controls the font style of the week labels at the top of the chart
	 *
	 * @param dayOfWeekLabel
	 */
	public void setDayOfWeekLabel(final TextStyle dayOfWeekLabel)
	{
		this.dayOfWeekLabel = dayOfWeekLabel;
	}
	
	public TextStyle getMonthLabel()
	{
		return this.monthLabel;
	}
	
	/**
	 * Style for the month labels
	 *
	 * @param monthLabel
	 */
	public void setMonthLabel(final TextStyle monthLabel)
	{
		this.monthLabel = monthLabel;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("cellColor: " + this.cellColor + ", ");
		str.append("cellSize: " + this.cellSize + ", ");
		str.append("dayOfWeekLabel: " + this.dayOfWeekLabel + ", ");
		str.append("monthLabel: " + this.monthLabel + " ");
		str.append("}");
		
		return str.toString();
	}

}


package com.rapidclipse.framework.server.charts.calendar;

import com.rapidclipse.framework.server.charts.config.BackgroundStyle;
import com.rapidclipse.framework.server.charts.config.TextStyle;


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

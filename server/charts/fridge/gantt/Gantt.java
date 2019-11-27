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
package com.rapidclipse.framework.server.charts.gantt;

import com.rapidclipse.framework.server.charts.TextStyle;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Gantt
{
	private Integer   barCornerRadius     = 2;
	private Integer   barHeight;
	private boolean   percentEnabled      = true;
	private boolean   shadowEnabled       = true;
	private String    shadowColor         = "#000";
	private Integer   shadowOffset        = 1;
	private Integer   trackHeight;
	private boolean   criticalPathEnabled = true;
	private TextStyle labelStyle;
	private Integer   labelMaxWidth       = 300;
	
	private String innerGridTrack;
	private String innerGridDarkTrack;
	
	private Arrow    arrow              = new Arrow();
	private GridLine innerGridHorizLine = new GridLine();
	
	public Integer getBarCornerRadius()
	{
		return this.barCornerRadius;
	}
	
	/**
	 * The radius for defining the curve of a bar's corners. <br>
	 *
	 * @param barCornerRadius
	 */
	public void setBarCornerRadius(final Integer barCornerRadius)
	{
		this.barCornerRadius = barCornerRadius;
	}
	
	public Integer getBarHeight()
	{
		return this.barHeight;
	}
	
	/**
	 * The height of the bars for tasks. <br>
	 *
	 * @param barHeight
	 */
	public void setBarHeight(final Integer barHeight)
	{
		this.barHeight = barHeight;
	}
	
	public boolean isPercentEnabled()
	{
		return this.percentEnabled;
	}
	
	/**
	 * Fills the task bar based on the percentage completed for the task. <br>
	 *
	 * @param percentEnabled
	 */
	public void setPercentEnabled(final boolean percentEnabled)
	{
		this.percentEnabled = percentEnabled;
	}
	
	public boolean isShadowEnabled()
	{
		return this.shadowEnabled;
	}
	
	/**
	 * If set to true, draws a shadow under each task bar which has dependencies.
	 * <br>
	 *
	 * @param shadowEnabled
	 */
	public void setShadowEnabled(final boolean shadowEnabled)
	{
		this.shadowEnabled = shadowEnabled;
	}
	
	public String getShadowColor()
	{
		return this.shadowColor;
	}
	
	/**
	 * Defines the color of the shadows under any task bar which has dependencies.
	 * <br>
	 *
	 * @param shadowColor
	 */
	public void setShadowColor(final String shadowColor)
	{
		this.shadowColor = shadowColor;
	}
	
	public Integer getShadowOffset()
	{
		return this.shadowOffset;
	}
	
	/**
	 * Defines the offset, in pixels, of the shadows under any task bar which has
	 * dependencies. <br>
	 *
	 * @param shadowOffset
	 */
	public void setShadowOffset(final Integer shadowOffset)
	{
		this.shadowOffset = shadowOffset;
	}
	
	public Integer getTrackHeight()
	{
		return this.trackHeight;
	}
	
	/**
	 * The height of the tracks. <br>
	 *
	 * @param trackHeight
	 */
	public void setTrackHeight(final Integer trackHeight)
	{
		this.trackHeight = trackHeight;
	}
	
	public boolean isCriticalPathEnabled()
	{
		return this.criticalPathEnabled;
	}
	
	/**
	 * If true any arrows on the critical path will be styled differently. <br>
	 *
	 * @param criticalPathEnabled
	 */
	public void setCriticalPathEnabled(final boolean criticalPathEnabled)
	{
		this.criticalPathEnabled = criticalPathEnabled;
	}
	
	public Arrow getArrow()
	{
		return this.arrow;
	}
	
	/**
	 * For Gantt Chart, Arrow controls the various properties of the arrows
	 * connecting tasks. <br>
	 *
	 * @param arrow
	 */
	public void setArrow(final Arrow arrow)
	{
		this.arrow = arrow;
	}
	
	public GridLine getInnerGridHorizLine()
	{
		return this.innerGridHorizLine;
	}
	
	/**
	 * Defines the style of the inner horizontal grid lines. <br>
	 *
	 * @param line
	 */
	public void setInnerGridHorizLine(final GridLine innerGridHorizLine)
	{
		this.innerGridHorizLine = innerGridHorizLine;
	}

	public String getInnerGridTrack()
	{
		return this.innerGridTrack;
	}

	/**
	 * Sets a Color for uneven lines
	 *
	 * @param color
	 *            fill color
	 */
	public void setInnerGridTrack(final String color)
	{
		this.innerGridTrack = color;
	}

	public String getInnerGridDarkTrack()
	{
		return this.innerGridDarkTrack;
	}

	/**
	 * Sets a Color for even lines
	 *
	 * @param color
	 *            fill color
	 */
	public void setInnerGridDarkTrack(final String color)
	{
		this.innerGridDarkTrack = color;
	}

	public TextStyle getLabelStyle()
	{
		return this.labelStyle;
	}

	/**
	 * Sets the Font Style for the label.
	 * Some Fonts will not fit in the label space
	 *
	 * @param labelStyle
	 */
	public void setLabelStyle(final TextStyle labelStyle)
	{
		this.labelStyle = labelStyle;
	}
	
	public Integer getLabelMaxWidth()
	{
		return this.labelMaxWidth;
	}
	
	public void setLabelMaxWidth(final Integer labelMaxWidth)
	{
		this.labelMaxWidth = labelMaxWidth;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("barCornerRadius: " + this.barCornerRadius + ",");
		str.append("barHeight: " + this.barHeight + ",");
		str.append("percentEnabled: " + this.percentEnabled + ",");
		str.append("shadowEnabled: " + this.shadowEnabled + ",");
		str.append("shadowColor: '" + this.shadowColor + "',");
		str.append("shadowOffset: " + this.shadowOffset + ",");
		str.append("trackHeight: " + this.trackHeight + ",");
		if(this.labelStyle != null)
		{
			str.append("labelStyle: " + this.labelStyle + ",");
		}
		str.append("labelMaxWidth: " + this.labelMaxWidth + ",");
		str.append("innerGridTrack: {fill: '" + this.innerGridTrack + "'},");
		str.append("innerGridDarkTrack: {fill: '" + this.innerGridDarkTrack + "'},");
		
		str.append("criticalPathEnabled: " + this.criticalPathEnabled + ",");
		str.append("arrow: " + this.arrow + ",");
		str.append("innerGridHorizLine: " + this.innerGridHorizLine);
		str.append("}");
		
		return str.toString();
	}
}
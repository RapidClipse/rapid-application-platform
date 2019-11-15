/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.charts.line;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.IntervalGlobal;
import com.rapidclipse.framework.server.charts.config.Options;
import com.rapidclipse.framework.server.charts.config.VAxis;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class XdevLineChartConfig extends AbstractXdevChartConfig implements Serializable
{
	
	private Integer        pointSize   = 0;
	private Integer        lineWidth   = 2;
	private List<Integer>  lineDashStyle;
	private HAxis          hAxis;
	private VAxis          vAxis;
	private String         curveType   = Options.CURVETYPE_NONE;
	private String         orientation = Options.ORIENTATION_HORIZONTAL;
	private String         pointShape  = Options.POINTSHAPE_CIRCLE;
	private IntervalGlobal intervals;
	
	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("pointSize", this.pointSize);
		options.put("lineWidth", this.lineWidth);
		options.put("lineDashStyle", this.lineDashStyle);
		options.put("hAxis", this.hAxis);
		options.put("vAxis", this.vAxis);
		options.put("pointShape", this.pointShape);
		options.put("curveType", this.curveType);
		options.put("orientation", this.orientation);
		if(this.intervals != null)
		{
			options.put("intervals", this.intervals);
		}
		return options;
	}

	public Integer getPointSize()
	{
		return this.pointSize;
	}

	/**
	 * Diameter of displayed points in pixels. Use zero to hide all points. <br>
	 *
	 * @param pointSize
	 */
	public void setPointSize(final Integer pointSize)
	{
		this.pointSize = pointSize;
	}

	public HAxis gethAxis()
	{
		return this.hAxis;
	}

	public IntervalGlobal getIntervals()
	{
		return this.intervals;
	}

	/**
	 * Sets a list of Intervals
	 * Intervals might be used to portray confidence intervals, minimum and maximum values around a value, percentile
	 * sampling, or anything else that requires a varying margin around a series<br>
	 * The first series of a line chart is the primary series, and the other will be compared to it via intervals
	 *
	 * @param intervals
	 */
	public void setIntervals(final IntervalGlobal intervals)
	{
		this.intervals = intervals;
	}

	public void sethAxis(final HAxis hAxis)
	{
		this.hAxis = hAxis;
	}

	public VAxis getvAxis()
	{
		return this.vAxis;
	}

	public void setvAxis(final VAxis vAxis)
	{
		this.vAxis = vAxis;
	}

	public String getCurveType()
	{
		return this.curveType;
	}

	/**
	 * Controls the curve of the lines when the line width is not zero. Can be one
	 * of the following: <br>
	 * <ul>
	 * <li>'none' - Straight lines without curve.</li>
	 * <li>'function' - The angles of the line will be smoothed.</li>
	 * </ul>
	 * <br>
	 *
	 * @param curveType
	 */
	public void setCurveType(final String curveType)
	{
		this.curveType = curveType;
	}

	public String getOrientation()
	{
		return this.orientation;
	}

	/**
	 * The orientation of the chart. When set to 'vertical', rotates the axes of the
	 * chart <br>
	 *
	 * @param orientation
	 */
	public void setOrientation(final String orientation)
	{
		this.orientation = orientation;
	}

	public String getPointShape()
	{
		return this.pointShape;
	}

	/**
	 * The shape of individual data elements: 'circle', 'triangle', 'square',
	 * 'diamond', 'star', or 'polygon'. <br>
	 *
	 * @param pointShape
	 */
	public void setPointShape(final String pointShape)
	{
		this.pointShape = pointShape;
	}

	public Integer getLineWidth()
	{
		return this.lineWidth;
	}

	/**
	 * Data line width in pixels. Use zero to hide all lines and show only the
	 * points. You can override values for individual series using the series
	 * property. <br>
	 *
	 * @param lineWidth
	 */
	public void setLineWidth(final Integer lineWidth)
	{
		this.lineWidth = lineWidth;
	}

	public List<Integer> getLineDashStyle()
	{
		return this.lineDashStyle;
	}

	/**
	 * The on-and-off pattern for dashed lines. For instance, [4, 4] will repeat
	 * 4-length dashes followed by 4-length gaps, and [5, 1, 3] will repeat a
	 * 5-length dash, a 1-length gap, a 3-length dash, a 5-length gap, a 1-length
	 * dash, and a 3-length gap.
	 *
	 * @param lineDashStyle
	 */
	public void setLineDashStyle(final List<Integer> lineDashStyle)
	{
		this.lineDashStyle = lineDashStyle;
	}
	
}

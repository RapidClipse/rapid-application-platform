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

package com.rapidclipse.framework.server.charts.combo;

import java.util.List;

import com.rapidclipse.framework.server.charts.config.Options;
import com.rapidclipse.framework.server.charts.config.XdevSeries;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevComboChartSeries extends XdevSeries
{

	private String        type            = "line";
	private double        areaOpacity     = 0.3;
	private String        color;
	private String        labelInLegend;
	private boolean       visibleInLegend = true;
	private Integer       lineWidth       = 2;
	private Integer       pointSize       = 0;
	private List<Integer> lineDashStyle;
	private String        curveType       = Options.CURVETYPE_NONE;
	private String        pointShape      = Options.POINTSHAPE_CIRCLE;

	public XdevComboChartSeries(final int num)
	{
		super(num);
	}

	/**
	 * @return the type
	 */
	public String getType()
	{
		return this.type;
	}

	/**
	 * The type of marker for this series. Valid values are 'line', 'area', 'bars'
	 * and 'steppedArea'. Note that bars are actually vertical bars (columns). The
	 * default value is specified by the chart's seriesType option. <br>
	 *
	 * @param type
	 *            the type to set
	 */
	public void setType(final String type)
	{
		this.type = type;
	}
	
	public double getAreaOpacity()
	{
		return this.areaOpacity;
	}
	
	/**
	 * Overrides the global areaOpacity for this series.
	 *
	 * @param areaOpacity
	 */
	public void setAreaOpacity(final double areaOpacity)
	{
		this.areaOpacity = areaOpacity;
	}
	
	/**
	 * @return the color
	 */
	public String getColor()
	{
		return this.color;
	}

	/**
	 * The color to use for this series. Specify a valid HTML color string. <br>
	 *
	 * @param color
	 *            the color to set
	 */
	public void setColor(final String color)
	{
		this.color = color;
	}

	/**
	 * @return the labelInLegend
	 */
	public String getLabelInLegend()
	{
		return this.labelInLegend;
	}

	/**
	 * The description of the series to appear in the chart legend. <br>
	 *
	 * @param labelInLegend
	 *            the labelInLegend to set
	 */
	public void setLabelInLegend(final String labelInLegend)
	{
		this.labelInLegend = labelInLegend;
	}

	/**
	 * @return the visibleInLegend
	 */
	public boolean isVisibleInLegend()
	{
		return this.visibleInLegend;
	}

	/**
	 * A boolean value, where true means that the series should have a legend entry,
	 * and false means that it should not. Default is true. <br>
	 *
	 * @param visibleInLegend
	 *            the visibleInLegend to set
	 */
	public void setVisibleInLegend(final boolean visibleInLegend)
	{
		this.visibleInLegend = visibleInLegend;
	}

	/**
	 * @return the lineWidth
	 */
	public Integer getLineWidth()
	{
		return this.lineWidth;
	}

	/**
	 * Data line width in pixels. Use zero to hide all lines and show only the
	 * points. <br>
	 *
	 * @param lineWidth
	 *            the lineWidth to set
	 */
	public void setLineWidth(final Integer lineWidth)
	{
		this.lineWidth = lineWidth;
	}

	/**
	 * @return the pointSize
	 */
	public Integer getPointSize()
	{
		return this.pointSize;
	}

	/**
	 * Diameter of displayed points in pixels. Use zero to hide all points. <br>
	 *
	 * @param pointSize
	 *            the pointSize to set
	 */
	public void setPointSize(final Integer pointSize)
	{
		this.pointSize = pointSize;
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
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("type: '" + this.type + "', ");
		str.append("areaOpacity: " + this.areaOpacity + ", ");
		if(this.color != null)
		{
			str.append("color: '" + this.color + "', ");
		}
		str.append("labelInLegend: '" + this.labelInLegend + "', ");
		str.append("visibleInLegend: " + this.visibleInLegend + ", ");
		str.append("lineWidth: " + this.lineWidth + ", ");
		str.append("pointSize: " + this.pointSize + ", ");
		if(this.lineDashStyle != null)
		{
			str.append("lineDashStyle: " + this.lineDashStyle + ", ");
		}

		str.append("curveType: '" + this.curveType + "', ");
		str.append("pointShape: '" + this.pointShape + "' ");
		str.append("}");

		return str.toString();
	}
	
}

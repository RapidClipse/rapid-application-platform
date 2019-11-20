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

package com.rapidclipse.framework.server.charts.scatter;

import com.rapidclipse.framework.server.charts.config.Options;
import com.rapidclipse.framework.server.charts.config.XdevSeries;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevScatterChartSeries extends XdevSeries
{

	private String  color;
	private String  labelInLegend;
	private boolean visibleInLegend = true;
	private Integer lineWidth       = 0;
	private Integer pointSize       = 8;
	private String  pointShape      = Options.POINTSHAPE_CIRCLE;

	public XdevScatterChartSeries(final int num)
	{
		super(num);
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

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		if(this.color != null)
		{
			str.append("color: '" + this.color + "', ");
		}
		str.append("labelInLegend: '" + this.labelInLegend + "', ");
		str.append("visibleInLegend: " + this.visibleInLegend + ", ");
		str.append("lineWidth: " + this.lineWidth + ", ");
		str.append("pointSize: " + this.pointSize + ", ");
		str.append("pointShape: '" + this.pointShape + "' ");
		str.append("}");

		return str.toString();
	}
	
}

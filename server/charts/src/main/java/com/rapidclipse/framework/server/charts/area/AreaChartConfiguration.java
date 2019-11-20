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
package com.rapidclipse.framework.server.charts.area;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;

import com.rapidclipse.framework.server.charts.ChartConfiguration;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.Options;
import com.rapidclipse.framework.server.charts.config.VAxis;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class AreaChartConfiguration extends ChartConfiguration implements Serializable
{
	
	private Integer       pointSize  = 0;
	private Integer       lineWidth  = 2;
	private List<Integer> lineDashStyle;
	private HAxis         hAxis;
	private VAxis         vAxis;
	private String        pointShape = Options.POINTSHAPE_CIRCLE;
	private boolean       isStacked  = false;

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
		options.put("isStacked", this.isStacked);
		return options;
	}
	
	public HAxis gethAxis()
	{
		return this.hAxis;
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

	/**
	 * @return the isStacked
	 */
	public boolean isStacked()
	{
		return this.isStacked;
	}

	/**
	 * If set to true, stacks the elements for all series at each domain value. <br>
	 *
	 * @param isStacked
	 *            the isStacked to set
	 */
	public void setStacked(final boolean isStacked)
	{
		this.isStacked = isStacked;
	}
	
}

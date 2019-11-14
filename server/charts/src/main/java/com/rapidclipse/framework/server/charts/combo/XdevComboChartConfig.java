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

package com.rapidclipse.framework.server.charts.combo;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;
import com.rapidclipse.framework.server.charts.config.Bar;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.Series;
import com.rapidclipse.framework.server.charts.config.VAxis;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 *
 */
public class XdevComboChartConfig extends AbstractXdevChartConfig implements Serializable
{
	private String  seriesType = "bars";
	private HAxis   hAxis;
	private VAxis   vAxis;
	private Series  series;
	private boolean interpolateNulls;
	private Bar     barGroupWidth;

	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("seriesType", this.seriesType);
		options.put("series", this.series);
		options.put("hAxis", this.hAxis);
		options.put("vAxis", this.vAxis);
		options.put("interpolateNulls", this.interpolateNulls);
		options.put("bar", this.barGroupWidth);
		return options;
	}
	
	/**
	 * @return the seriesType
	 */
	public String getSeriesType()
	{
		return this.seriesType;
	}

	/**
	 * The default line type for any series not specified in the series property.
	 * Available values are 'line', 'area', 'bars' and 'steppedArea'. <br>
	 *
	 * @param seriesType
	 *            the seriesType to set
	 */
	public void setSeriesType(final String seriesType)
	{
		this.seriesType = seriesType;
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
	
	public Series getSeries()
	{
		return this.series;
	}
	
	public void setSeries(final Series series)
	{
		this.series = series;
	}
	
	public boolean isInterpolateNulls()
	{
		return this.interpolateNulls;
	}
	
	public void setInterpolateNulls(final boolean interpolateNulls)
	{
		this.interpolateNulls = interpolateNulls;
	}
	
	public Bar getBarGroupWidth()
	{
		return this.barGroupWidth;
	}
	
	/**
	 * The width of a group of bars , specified in either of these formats:
	 * <li>Pixels (e.g. 50)</li>
	 * <li>Percentage of the available width for each group (e.g. '20%'), where '100%' means that groups have no space
	 * between them.</li>
	 * 
	 * @param barGroupWidth
	 *            String
	 */
	public void setBarGroupWidth(final String barGroupWidth)
	{
		this.barGroupWidth = new Bar(barGroupWidth);
	}
	
}

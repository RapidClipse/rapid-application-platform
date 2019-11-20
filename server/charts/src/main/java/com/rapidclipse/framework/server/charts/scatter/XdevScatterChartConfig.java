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

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.Series;
import com.rapidclipse.framework.server.charts.config.VAxis;
import com.rapidclipse.framework.server.charts.config.XdevTrendlines;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevScatterChartConfig extends AbstractXdevChartConfig implements Serializable
{

	private HAxis          hAxis;
	private VAxis          vAxis;
	private Series         series;
	private XdevTrendlines trendlines;

	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("hAxis", this.hAxis);
		options.put("vAxis", this.vAxis);
		options.put("series", this.series);
		options.put("trendlines", this.trendlines);
		
		return options;
	}
	
	public XdevTrendlines getTrendline()
	{
		return this.trendlines;
	}
	
	/**
	 * A trendline is a line superimposed on a chart revealing the overall direction of the data.
	 *
	 * @param trendline
	 */
	public void setTrendline(final XdevTrendlines trendline)
	{
		this.trendlines = trendline;
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

}

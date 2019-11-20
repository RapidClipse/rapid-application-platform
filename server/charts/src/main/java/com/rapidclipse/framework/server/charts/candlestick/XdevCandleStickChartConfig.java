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

package com.rapidclipse.framework.server.charts.candlestick;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;
import com.rapidclipse.framework.server.charts.config.Bar;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.VAxis;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class XdevCandleStickChartConfig extends AbstractXdevChartConfig implements Serializable
{
	private HAxis       hAxis;
	private VAxis       vAxis;
	private String      orientation = "horizontal";
	private Candlestick candlestick;
	private Bar         barGroupWidth;
	
	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("hAxis", this.hAxis);
		options.put("vAxis", this.vAxis);
		options.put("orientation", this.orientation);
		options.put("candlestick", this.candlestick);
		options.put("bar", this.barGroupWidth);
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
	
	public String getOrientation()
	{
		return this.orientation;
	}

	public Candlestick getCandlestick()
	{
		return this.candlestick;
	}

	/**
	 * for RisingColor or fallingColor
	 *
	 * @param candlestick
	 */
	public void setCandlestick(final Candlestick candlestick)
	{
		this.candlestick = candlestick;
	}

	/**
	 * The orientation of the chart.
	 *
	 * @param orientation
	 *            'horizontal' or 'vertical'
	 */
	public void setOrientation(final String orientation)
	{
		this.orientation = orientation;
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

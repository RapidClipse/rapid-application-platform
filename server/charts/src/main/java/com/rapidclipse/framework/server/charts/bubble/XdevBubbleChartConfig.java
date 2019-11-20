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

package com.rapidclipse.framework.server.charts.bubble;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;
import com.rapidclipse.framework.server.charts.config.ColorAxis;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.Options;
import com.rapidclipse.framework.server.charts.config.VAxis;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class XdevBubbleChartConfig extends AbstractXdevChartConfig implements Serializable
{
	private HAxis     hAxis;
	private VAxis     vAxis;
	private String    axisTitlesPosition = Options.TEXTPOSITION_OUT;
	private ColorAxis colorAxis;
	private Bubble    bubble             = new Bubble();
	
	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("hAxis", this.hAxis);
		options.put("vAxis", this.vAxis);
		options.put("axisTitlesPosition", this.axisTitlesPosition);
		options.put("colorAxis", this.colorAxis);
		options.put("bubble", this.bubble);
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

	public ColorAxis getColorAxis()
	{
		return this.colorAxis;
	}
	
	/**
	 * An object that specifies a mapping between color column values and colors or a gradient scale.
	 * 
	 * @param colorAxis
	 */
	public void setColorAxis(final ColorAxis colorAxis)
	{
		this.colorAxis = colorAxis;
	}
	
	public VAxis getvAxis()
	{
		return this.vAxis;
	}

	public void setvAxis(final VAxis vAxis)
	{
		this.vAxis = vAxis;
	}

	public String getAxisTitlesPosition()
	{
		return this.axisTitlesPosition;
	}

	/**
	 * Where to place the axis titles, compared to the chart area. Supported values:
	 * <ul>
	 * <li>in - Draw the axis titles inside the chart area.</li>
	 * <li>out - Draw the axis titles outside the chart area.</li>
	 * <li>none - Omit the axis titles.</li>
	 * </ul>
	 * <br>
	 *
	 * @param axisTitlesPosition
	 */
	public void setAxisTitlesPosition(final String axisTitlesPosition)
	{
		this.axisTitlesPosition = axisTitlesPosition;
	}

	public Bubble getBubble()
	{
		return this.bubble;
	}

	/**
	 * An object with members to configure the visual properties of the bubbles.
	 * <br>
	 *
	 * @param bubble
	 */
	public void setBubble(final Bubble bubble)
	{
		this.bubble = bubble;
	}
	
}

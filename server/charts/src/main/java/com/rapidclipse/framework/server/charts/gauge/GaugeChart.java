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

package com.rapidclipse.framework.server.charts.gauge;

import java.util.List;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasValueRange;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("gauge-chart")
public class GaugeChart extends ChartBase
	implements HasAnimation, AllowsIFrame, HasChartSize, HasValueRange
{
	public GaugeChart()
	{
		super("GaugeChart", "gauge");
	}
	
	public ChartModel initDefaultColumns()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "label"))
			.addColumn(Column.New(Column.Type.NUMBER, "value"));
	}

	public String getGreenColor()
	{
		return properties().get("greenColor");
	}

	public void setGreenColor(final String greenColor)
	{
		properties().put("greenColor", greenColor);
	}

	public Double getGreenFrom()
	{
		return properties().get("greenFrom");
	}

	public void setGreenFrom(final Double greenFrom)
	{
		properties().put("greenFrom", greenFrom);
	}

	public Double getGreenTo()
	{
		return properties().get("greenTo");
	}

	public void setGreenTo(final Double greenTo)
	{
		properties().put("greenTo", greenTo);
	}

	public String getRedColor()
	{
		return properties().get("redColor");
	}

	public void setRedColor(final String redColor)
	{
		properties().put("redColor", redColor);
	}

	public Double getRedFrom()
	{
		return properties().get("redFrom");
	}

	public void setRedFrom(final Double redFrom)
	{
		properties().put("redFrom", redFrom);
	}

	public Double getRedTo()
	{
		return properties().get("redTo");
	}

	public void setRedTo(final Double redTo)
	{
		properties().put("redTo", redTo);
	}

	public String getYellowColor()
	{
		return properties().get("yellowColor");
	}

	public void setYellowColor(final String yellowColor)
	{
		properties().put("yellowColor", yellowColor);
	}

	public Double getYellowFrom()
	{
		return properties().get("yellowFrom");
	}

	public void setYellowFrom(final Double yellowFrom)
	{
		properties().put("yellowFrom", yellowFrom);
	}

	public Double getYellowTo()
	{
		return properties().get("yellowTo");
	}

	public void setYellowTo(final Double yellowTo)
	{
		properties().put("yellowTo", yellowTo);
	}

	public List<String> getMajorTicks()
	{
		return properties().get("majorTicks");
	}

	public void setMajorTicks(final List<String> majorTicks)
	{
		properties().put("majorTicks", majorTicks);
	}

	public List<String> getMinorTicks()
	{
		return properties().get("minorTicks");
	}

	public void setMinorTicks(final List<String> minorTicks)
	{
		properties().put("minorTicks", minorTicks);
	}
}

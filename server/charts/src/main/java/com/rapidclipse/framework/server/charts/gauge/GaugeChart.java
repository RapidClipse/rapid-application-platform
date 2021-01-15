/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.gauge;

import java.util.List;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
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
public class GaugeChart extends AbstractChart
	implements HasAnimation, AllowsIFrame, HasChartSize, HasValueRange
{
	public GaugeChart()
	{
		super("Gauge", "gauge");
	}

	public ChartModel initDefaultColumnsSimple()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "label"))
			.addColumn(Column.New(Column.Type.NUMBER, "value"));
	}

	public ChartModel initDefaultColumnsMulti(final String... valueColumns)
	{
		final ChartModel model = getModel().removeAll();
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}

	public String getGreenColor()
	{
		return properties().get("greenColor", "#109618");
	}

	public void setGreenColor(final String greenColor)
	{
		properties().put("greenColor", greenColor);
	}

	public Number getGreenFrom()
	{
		return properties().get("greenFrom", null);
	}

	public void setGreenFrom(final Number greenFrom)
	{
		properties().put("greenFrom", greenFrom);
	}

	public Number getGreenTo()
	{
		return properties().get("greenTo", null);
	}

	public void setGreenTo(final Number greenTo)
	{
		properties().put("greenTo", greenTo);
	}

	public String getRedColor()
	{
		return properties().get("redColor", "#DC3912");
	}

	public void setRedColor(final String redColor)
	{
		properties().put("redColor", redColor);
	}

	public Number getRedFrom()
	{
		return properties().get("redFrom", null);
	}

	public void setRedFrom(final Number redFrom)
	{
		properties().put("redFrom", redFrom);
	}

	public Number getRedTo()
	{
		return properties().get("redTo", null);
	}

	public void setRedTo(final Number redTo)
	{
		properties().put("redTo", redTo);
	}

	public String getYellowColor()
	{
		return properties().get("yellowColor", "#FF9900");
	}

	public void setYellowColor(final String yellowColor)
	{
		properties().put("yellowColor", yellowColor);
	}

	public Number getYellowFrom()
	{
		return properties().get("yellowFrom", null);
	}

	public void setYellowFrom(final Number yellowFrom)
	{
		properties().put("yellowFrom", yellowFrom);
	}

	public Number getYellowTo()
	{
		return properties().get("yellowTo", null);
	}

	public void setYellowTo(final Number yellowTo)
	{
		properties().put("yellowTo", yellowTo);
	}

	public List<String> getMajorTicks()
	{
		return properties().get("majorTicks", null);
	}

	public void setMajorTicks(final List<String> majorTicks)
	{
		properties().put("majorTicks", majorTicks);
	}

	public Number getMinorTicks()
	{
		return properties().get("minorTicks", 2);
	}

	public void setMinorTicks(final Number minorTicks)
	{
		properties().put("minorTicks", minorTicks);
	}

	@Override
	public Number getMin()
	{
		return properties().get("min", 0);
	}

	@Override
	public Number getMax()
	{
		return properties().get("max", 100);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumnsSimple()
			.addRow("Memory", 80)
			.addRow("CPU", 55)
			.addRow("Network", 68);

		setYellowFrom(75);
		setYellowTo(90);
		setRedFrom(90);
		setRedTo(100);
		setMinorTicks(5);
	}
}

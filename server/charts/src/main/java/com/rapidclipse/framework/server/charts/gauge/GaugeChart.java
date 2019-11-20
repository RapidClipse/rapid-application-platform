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

import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Value;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("gauge-chart")
public class GaugeChart extends Chart
{
	private GaugeChartConfig config;
	private DataTable        dataTable;

	public GaugeChart()
	{
		super();

		this.config = new GaugeChartConfig();
	}
	
	/**
	 * Override the default options
	 *
	 * @param config
	 */
	public void setConfig(final GaugeChartConfig config)
	{
		this.config = config;
	}
	
	/**
	 * Set a model for the chart
	 *
	 * @param model
	 */
	public void setModel(final ChartModel model)
	{
		this.dataTable = model.getDataTable();
		this.buildChart();
	}

	/**
	 *
	 * @param gauge
	 *            0 for the first one
	 * @param value
	 *            new value
	 */
	public void changeValue(final int gauge, final int value)
	{
		final ChartJsBuilder js   = new ChartJsBuilder(this.dataTable,
			this.config.getOptions(), this.id(), "Gauge");
		final Page           page = UI.getCurrent().getPage();

		String gaugeJs = js.constructChart();
		gaugeJs  = (String)gaugeJs.subSequence(0, gaugeJs.length() - 1);
		gaugeJs += "data.setValue(" + gauge + ", 1, " + value
			+ "); var view = new google.visualization.DataView(data); chart.draw(view, options); }";
		
		page.executeJs(gaugeJs);

		this.dataTable.getRows().get(gauge).getC().set(1, new Value(value));
	}

	/**
	 * Draws the chart.
	 * setModel or buildChart should be the last methods to call.
	 */
	public void buildChart()
	{
		final ChartJsBuilder js   = new ChartJsBuilder(this.dataTable,
			this.config.getOptions(), this.id(), "Gauge");
		final Page           page = UI.getCurrent().getPage();
		page.executeJs(js.constructChart());
	}
}

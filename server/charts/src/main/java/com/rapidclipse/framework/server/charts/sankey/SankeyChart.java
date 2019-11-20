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

package com.rapidclipse.framework.server.charts.sankey;

import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("sankey-chart")
public class SankeyChart extends Chart
{
	private SankeyChartConfig config;
	private DataTable         dataTable;

	public SankeyChart()
	{
		super("Sankey", "sankey");
		
		this.config = new SankeyChartConfig();
	}
	
	/**
	 * Override the default options
	 *
	 * @param config
	 */
	public void setConfig(final SankeyChartConfig config)
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
	 * Draws the chart.
	 * setModel or buildChart should be the last methods to call.
	 */
	public void buildChart()
	{
		final ChartJsBuilder js   = new ChartJsBuilder(this.dataTable,
			this.config.getOptions(), this.id(), "Sankey");
		final Page           page = UI.getCurrent().getPage();
		page.executeJs(js.constructChart());
	}
}

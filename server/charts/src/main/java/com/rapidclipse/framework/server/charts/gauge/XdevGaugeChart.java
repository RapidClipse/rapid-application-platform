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

import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.config.IdGenerator;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Value;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("Area-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevGaugeChart extends Composite<Div> implements HasSize
{
	private final String         id;
	private XdevGaugeChartConfig config;
	private DataTable            dataTable;
	
	public XdevGaugeChart()
	{
		super();
		this.id = IdGenerator.generateId();
		
		this.config = new XdevGaugeChartConfig();
	}

	/**
	 * Override the default options
	 *
	 * @param config
	 */
	public void setConfig(final XdevGaugeChartConfig config)
	{
		this.config = config;
	}

	/**
	 * Set a model for the chart
	 *
	 * @param model
	 */
	public void setModel(final XdevChartModel model)
	{
		this.dataTable = model.getDataTable();
		this.setId(this.id);
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
			this.config.getOptions(), this.id, "Gauge");
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
			this.config.getOptions(), this.id, "Gauge");
		final Page           page = UI.getCurrent().getPage();
		page.executeJs(js.constructChart());
	}
}

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

package com.rapidclipse.framework.server.charts.gantt;

import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.config.IdGenerator;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
@Tag("Gantt-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevGanttChart extends Composite<Div> implements HasSize
{
	private final GanttChartComponentState ganttState = new GanttChartComponentState();
	private final String                   id;
	
	public XdevGanttChart()
	{
		super();
		this.id = IdGenerator.generateId();
		this.ganttState.setConfig(new XdevGanttChartConfig());
	}
	
	/**
	 * Override the default options
	 *
	 * @param config
	 */
	public void setConfig(final XdevGanttChartConfig config)
	{
		this.ganttState.setConfig(config);
	}
	
	/**
	 * Set a model for the chart
	 *
	 * @param model
	 */
	public void setModel(final XdevChartModel model)
	{
		this.ganttState.setDataTable(model.getDataTable());
		this.setId(this.id);
		this.buildChart();
	}

	/**
	 * Draws the chart.
	 * setModel or buildChart should be the last methods to call.
	 */
	public void buildChart()
	{
		final ChartJsBuilder js   = new ChartJsBuilder(this.ganttState.getDataTable(),
			this.ganttState.getConfig().getOptions(), this.id, "Gantt");
		final Page           page = UI.getCurrent().getPage();
		page.executeJs(js.constructChart());
	}
}

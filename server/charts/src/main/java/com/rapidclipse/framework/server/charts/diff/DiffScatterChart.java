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

package com.rapidclipse.framework.server.charts.diff;

import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.config.IdGenerator;
import com.rapidclipse.framework.server.charts.config.Series;
import com.rapidclipse.framework.server.charts.config.Line;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;
import com.rapidclipse.framework.server.charts.scatter.ScatterChartConfig;
import com.rapidclipse.framework.server.charts.scatter.ScatterChartModel;
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
@Tag("diff-Scatter-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class DiffScatterChart extends Composite<Div> implements HasSize
{
	private DataTable              oldData;
	private DataTable              newData;
	private Series                 series;
	private final String           id;
	private ScatterChartConfig config;

	public DiffScatterChart()
	{
		super();
		this.id     = IdGenerator.generateId();
		this.config = new ScatterChartConfig();
		this.setId(this.id);
	}
	
	private DataTable setDataTable(final ChartModel model)
	{
		final ScatterChartModel chartModel = (ScatterChartModel)model;

		if(this.series == null)
		{
			for(final Line ser : chartModel.getSeries())
			{
				if(ser.getNum() == 0)
				{
					ser.setNum(1);
				}
				else
				{
					ser.setNum((ser.getNum() * 2) + 1);
				}
			}
			this.series = new Series(chartModel.getSeries());
		}
		Row.createFromHashmap(chartModel.getData()).forEach(row -> chartModel.getDataTable().getRows().add(row));

		this.config.setSeries(this.series);
		return chartModel.getDataTable();
		
	}

	public void setModel(final ChartModel oldModel, final ChartModel newModel)
	{
		this.oldData = this.setDataTable(oldModel);
		this.newData = this.setDataTable(newModel);
		this.buildChart();
	}
	
	public void setConfig(final ScatterChartConfig config)
	{
		this.config = config;
		this.config.setSeries(this.series);
		
	}
	
	public void buildChart()
	{
		final ChartJsBuilder oldBuild = new ChartJsBuilder(this.oldData,
			this.config.getOptions(), this.id, "ScatterChart");
		final ChartJsBuilder newBuild = new ChartJsBuilder(this.newData,
			this.config.getOptions(), this.id, "ScatterChart");
		final StringBuilder  bld      = new StringBuilder();
		bld.append(newBuild.makeFunction());
		bld.append(newBuild.makeOptions());
		bld.append(oldBuild.makeDataTable("oldData"));
		bld.append(newBuild.makeDataTable("newData"));
		bld.append("var diffData = chart.computeDiff(oldData, newData); ");
		bld.append("chart.draw(diffData , options); }");
		final Page page = UI.getCurrent().getPage();
		page.executeJs(bld.toString());
	}
	
}

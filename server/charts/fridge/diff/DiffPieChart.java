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

import java.util.List;

import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.config.Slices;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.pie.PieChartConfiguration;
import com.rapidclipse.framework.server.charts.pie.PieChartModel;
import com.rapidclipse.framework.server.charts.pie.Slice;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("diff-pie-chart")
public class DiffPieChart extends Chart
{
	private DataTable      oldData;
	private DataTable      newData;
	private List<Slice> slices;
	private PieChartConfiguration config;
	
	public DiffPieChart()
	{
		super("PieChart", "corechart");
		
		this.config = new PieChartConfiguration();

	}

	private DataTable setDataTable(final ChartModel model)
	{
		final PieChartModel pieModel = (PieChartModel)model;
		
		final DataTable table = new DataTable();
		table.setColumns(pieModel.getDataTable().getColumns());
		table.setRows(pieModel.getDataTable().getRows());
		if(this.slices == null)
		{
			this.slices = pieModel.getSlices();
			this.config.setSlices(new Slices(this.slices));
		}
		return table;

	}
	
	public void setModel(final ChartModel oldModel, final ChartModel newModel)
	{
		this.oldData = this.setDataTable(oldModel);
		this.newData = this.setDataTable(newModel);
		this.buildChart();
	}

	public void setConfig(final PieChartConfiguration config)
	{
		this.config = config;
		if(this.slices != null)
		{
			this.config.setSlices(new Slices(this.slices));
		}
	}

	public void buildChart()
	{
		final ChartJsBuilder oldBuild = new ChartJsBuilder(this.oldData,
			this.config.getOptions(), this.id(), "PieChart");
		final ChartJsBuilder newBuild = new ChartJsBuilder(this.newData,
			this.config.getOptions(), this.id(), "PieChart");
		final StringBuilder  bld      = new StringBuilder();
		bld.append(newBuild.makeFunction());
		bld.append(newBuild.makeOptions());
		bld.append(oldBuild.makeDataTable("oldData"));
		bld.append(newBuild.makeDataTable("newData"));
		bld.append("var diffData = chart.computeDiff(oldData, newData); ");
		bld.append("chart.draw(diffData, options); }");
		final Page page = UI.getCurrent().getPage();
		page.executeJs(bld.toString());
	}
}

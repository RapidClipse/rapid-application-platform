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

import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.column.ColumnChartConfiguration;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("diff-column-chart")
public class DiffColumnChart extends Chart
{
	private DataTable         oldData;
	private DataTable         newData;
	private ColumnChartConfiguration config;

	public DiffColumnChart()
	{
		super("ColumnChart", "corechart");

		this.config = new ColumnChartConfiguration();
	}
	
	private DataTable setDataTable(final ChartModel model)
	{
		Row.createFromHashmap(model.getData()).forEach(row -> model.getDataTable().getRows().add(row));

		return model.getDataTable();
		
	}

	public void setModel(final ChartModel oldModel, final ChartModel newModel)
	{
		this.oldData = this.setDataTable(oldModel);
		this.newData = this.setDataTable(newModel);
		this.buildChart();
	}
	
	public void setConfig(final ColumnChartConfiguration config)
	{
		this.config = config;
		
	}
	
	public void buildChart()
	{
		final ChartJsBuilder oldBuild = new ChartJsBuilder(this.oldData,
			this.config.getOptions(), this.id(), "ColumnChart");
		final ChartJsBuilder newBuild = new ChartJsBuilder(this.newData,
			this.config.getOptions(), this.id(), "ColumnChart");
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
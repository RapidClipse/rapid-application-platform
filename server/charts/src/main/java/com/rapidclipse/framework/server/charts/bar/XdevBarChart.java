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

package com.rapidclipse.framework.server.charts.bar;

import java.util.Optional;

import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.config.IdGenerator;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
@Tag("Bar-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevBarChart extends Component
{
	private final BarChartComponentState barState = new BarChartComponentState();
	
	private final String id;
	
	public XdevBarChart()
	{
		super();
		this.id = IdGenerator.generateId();
		
		this.barState.setConfig(new XdevBarChartConfig());
	}
	
	/**
	 * Override the default options
	 *
	 * @param config
	 */
	public void setConfig(final XdevBarChartConfig config)
	{
		this.barState.setConfig(config);
	}

	/**
	 * Set a model for the chart
	 *
	 * @param model
	 */
	public void setModel(final XdevChartModel model)
	{
		final DataTable table = new DataTable();

		model.getDataTable().getColumns().forEach(column -> table.getColumns().add(column));
		
		Row.createFromHashmap(model.getData()).forEach(row -> table.getRows().add(row));
		
		this.barState.setDataTable(table);
		
		final Optional<Component> parent = this.getParent();
		if(parent.isPresent())
		{
			parent.get().setId(this.id);
		}
		else
		{
			this.setId(this.id);
		}
		this.buildChart();

	}

	public void buildChart()
	{
		final ChartJsBuilder js   = new ChartJsBuilder(this.barState.getDataTable(),
			this.barState.getConfig().getOptions(), this.id, "BarChart");
		final Page           page = UI.getCurrent().getPage();
		page.executeJs(js.constructChart());
	}

}

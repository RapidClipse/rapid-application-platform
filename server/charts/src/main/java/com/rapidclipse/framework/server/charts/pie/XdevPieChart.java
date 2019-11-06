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

package com.rapidclipse.framework.server.charts.pie;

import java.util.Optional;

import com.rapidclipse.framework.server.charts.DataTable;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 *
 *
 *        TODO: connect with XdevChart
 *        DONE: JavaScriptComponentState.getState() dosn't exist anymore, new PieChartComponentState object needed
 */
@Tag("pie-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevPieChart extends Component
{
	private final PieChartComponentState pieState = new PieChartComponentState();
	private final XdevPieChartJsBuilder  jsBuilder;
	private final String                 id;
	
	public XdevPieChart()
	{
		super();
		this.id = this.idGenerator();
		
		this.pieState.setConfig(new XdevPieChartConfig());
		this.jsBuilder = new XdevPieChartJsBuilder(this.pieState, this.id);

	}

	public void setConfig(final XdevPieChartConfig config)
	{
		this.pieState.setConfig(config);
	}
	
	public void setModel(final XdevChartModel model)
	{
		final XdevPieChartModel pieModel = (XdevPieChartModel)model;
		
		final DataTable table = new DataTable();
		table.setColumns(pieModel.getDataTable().getColumns());
		table.setRows(pieModel.getDataTable().getRows());

		this.pieState.setDataTable(table);
		this.pieState.setSlices(pieModel.getSlices());
		
		final Optional<Component> parent = this.getParent();
		if(parent.isPresent())
		{
			parent.get().setId(this.id);
		}
		else
		{
			this.setId(this.id);
		}
		final Page page = UI.getCurrent().getPage();
		page.executeJs(this.jsBuilder.constructChart());
	}

	private String idGenerator()
	{
		final String        possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
		final StringBuilder idStr    = new StringBuilder();

		for(int i = 0; i < 10; i++)
		{
			idStr.append(possible.charAt((int)Math.floor(Math.random() * possible.length())));
		}
		return idStr.toString();
	}
	
}

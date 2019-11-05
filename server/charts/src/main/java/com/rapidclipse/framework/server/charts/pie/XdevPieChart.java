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

import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.DataTable;
import com.rapidclipse.framework.server.charts.Row;
import com.rapidclipse.framework.server.charts.Value;
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
	private final String                 id;
	
	public XdevPieChart()
	{
		super();
		this.id = this.idGenerator();
		this.setId(this.id);
		this.pieState.setConfig(new XdevPieChartConfig());
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
	}

	private String idGenerator()
	{
		String       idg      = " ";
		final String possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
		for(int i = 0; i < 10; i++)
		{
			idg += possible.charAt((int)Math.floor(Math.random() * possible.length()));
		}
		return idg;
	}

	// TODO: Listener or trigger?
	// - new model
	// - model changed?
	public void constructChart()
	{
		final Page          page = UI.getCurrent().getPage();
		final StringBuilder bld  = new StringBuilder();
		
		bld.append(this.makeFunction());
		bld.append(this.makeOptions());
		bld.append(this.makeDataTable());
		bld.append("chart.draw(view, options); }");

		final String executeString = bld.toString();
		page.executeJs(executeString);
	}

	private String makeFunction()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("google.charts.load('visualization', 'current', {'packages': ['corechart']}); ");
		bld.append("google.charts.setOnLoadCallback(drawChart); ");
		bld.append("function drawChart(){ ");
		bld.append("var chart = new google.visualization.PieChart(document.getElementById('" + this.id + "')); ");
		
		return bld.toString();
	}

	private String makeOptions()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var options ={ ");

		bld.append("title: " + this.pieState.getConfig().getTitle() + ", ");
		bld.append("titleTextStyle: " + this.pieState.getConfig().getTitleTextStyle() + ", ");
		bld.append("is3D: " + this.pieState.getConfig().getIs3D() + ", ");
		bld.append("pieHole: " + this.pieState.getConfig().getPieHole() + ", ");
		bld.append("backgroundColor: " + this.pieState.getConfig().getBackgroundColor() + ", ");
		bld.append("fontName: '" + this.pieState.getConfig().getFontName() + "', ");
		bld.append("fontSize: " + this.pieState.getConfig().getFontSize() + ", ");
		bld.append("pieSliceText: '" + this.pieState.getConfig().getPieSliceText() + "', ");
		bld.append("pieSliceTextStyle: " + this.pieState.getConfig().getPieSliceTextStyle() + ", ");
		bld.append("legend: " + this.pieState.getConfig().getLegend() + ", ");
		bld.append("chartArea: " + this.pieState.getConfig().getChartArea() + ", ");
		bld.append("pieSliceBorderColor: '" + this.pieState.getConfig().getPieSliceBorderColor() + "', ");
		bld.append("sliceVisibilityThreshold: " + this.pieState.getConfig().getSliceVisibilityThreshold() + ", ");
		bld.append("pieResidueSliceColor: '" + this.pieState.getConfig().getPieResidueSliceColor() + "', ");
		bld.append("pieResidueSliceLabel: '" + this.pieState.getConfig().getPieResidueSliceLabel() + "', ");
		bld.append("slices:" + this.pieState.getSlices() + ", ");
		bld.append("tooltip:" + this.pieState.getConfig().getTooltip() + ", ");
		bld.append("pieStartAngle: " + this.pieState.getConfig().getPieStartAngle());

		bld.append("}; ");
		
		return bld.toString();
	}
	
	private String makeDataTable()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var data = new google.visualization.DataTable(); ");
		
		for(final Column col : this.pieState.getDataTable().getColumns())
		{
			bld.append("data.addColumn(" + col.jsPrint() + "); ");
		}
		
		bld.append("data.addRows([");
		int i = 1;
		for(final Row row : this.pieState.getDataTable().getRows())
		{
			bld.append("[");
			int j = 1;
			for(final Value v : row.getC())
			{
				bld.append(v);
				if(j != row.getC().size())
				{
					bld.append(", ");
				}
				j++;
			}
			bld.append("]");
			if(i != this.pieState.getDataTable().getRows().size())
			{
				bld.append(", ");
			}
			i++;
		}
		bld.append(" ]); ");
		bld.append("var view = new google.visualization.DataView(data); ");
		return bld.toString();
	}
	
}

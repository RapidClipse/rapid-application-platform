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

package com.rapidclipse.framework.server.charts;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;
import com.rapidclipse.framework.server.charts.data.Value;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class ChartJsBuilder
{
	private DataTable                     data  = new DataTable();
	private final HashMap<String, Object> options;
	private final String                  id;
	private final String                  type;
	private String                        apiKey;
	private final List<String>            other =
		new ArrayList<>(Arrays.asList("gantt", "orgchart", "timeline", "calendar", "gauge", "sankey"));
	private final List<String>            maps  =
		new ArrayList<>(Arrays.asList("geochart", "map"));
	
	/**
	 *
	 * @param data
	 * @param options
	 * @param id
	 * @param type
	 *            Type of the Chart like "BarChart"
	 */
	public ChartJsBuilder(
		final DataTable data,
		final HashMap<String, Object> options,
		final String id,
		final String type)
	{
		super();
		this.data    = data;
		this.options = options;
		this.id      = id;
		this.type    = type;
	}
	
	public ChartJsBuilder(
		final DataTable data,
		final HashMap<String, Object> options,
		final String id,
		final String type,
		final String apiKey)
	{
		super();
		this.data    = data;
		this.options = options;
		this.id      = id;
		this.type    = type;
		this.apiKey  = apiKey;
	}
	
	/**
	 * Building a String for executeJs to make a JS page for the Chart
	 *
	 * @return String
	 */
	public String constructChart()
	{
		final StringBuilder bld = new StringBuilder();
		
		bld.append(this.makeFunction());
		bld.append(this.makeOptions());
		bld.append(this.makeDataTable("data"));
		bld.append("var view = new google.visualization.DataView(data); ");
		bld.append("chart.draw(view, options); }");
		
		return bld.toString();
	}

	/**
	 * phase 1
	 *
	 * @return
	 */
	public String makeFunction()
	{
		final StringBuilder bld = new StringBuilder();
		if(this.other.contains(this.type.toLowerCase()))
		{
			bld.append(
				"google.charts.load('visualization', 'current', {'packages': ['" + this.type.toLowerCase() + "']}); ");

		}
		else if(this.maps.contains(this.type.toLowerCase()))
		{
			bld.append(
				"google.charts.load('visualization', 'current', {'packages': ['" + this.type.toLowerCase()
					+ "'], 'mapsApiKey': '" + this.apiKey + "'}); ");
		}
		else
		{
			bld.append("google.charts.load('visualization', 'current', {'packages': ['corechart']}); ");
		}
		bld.append("google.charts.setOnLoadCallback(drawChart); ");
		bld.append("function drawChart(){ ");
		bld.append(
			"var chart = new google.visualization." + this.type + "(document.getElementById('" + this.id + "')); ");
		
		return bld.toString();
	}

	/**
	 * phase 2
	 *
	 * @return
	 */
	public String makeOptions()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var options ={   ");

		for(final Map.Entry<String, Object> entry : this.options.entrySet())
		{
			bld.append(entry.getKey() + ": ");
			if(entry.getValue() instanceof String)
			{
				bld.append("'" + entry.getValue() + "', ");
			}
			else
			{
				bld.append(entry.getValue() + ", ");
			}
		}
		bld.delete(bld.length() - 2, bld.length());
		bld.append("}; ");
		return bld.toString();
	}
	
	/**
	 * phase 3
	 *
	 * @return
	 */
	public String makeDataTable(final String dataName)
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var " + dataName + " = new google.visualization.DataTable(); ");
		
		for(final Column col : this.data.getColumns())
		{
			bld.append(dataName + ".addColumn(" + col.jsPrint() + "); ");
		}
		
		bld.append(dataName + ".addRows([");
		for(final Row row : this.data.getRows())
		{
			bld.append("[  ");
			for(final Value v : row.getC())
			{
				bld.append(v);
				bld.append(", ");
			}
			bld.delete(bld.length() - 2, bld.length());
			bld.append("]");
			bld.append(", ");
		}
		bld.delete(bld.length() - 2, bld.length());
		bld.append(" ]); ");
		
		return bld.toString();
	}
}

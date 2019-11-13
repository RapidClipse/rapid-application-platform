
package com.rapidclipse.framework.server.charts;

import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;
import com.rapidclipse.framework.server.charts.data.Value;


public class ChartJsBuilder
{
	private DataTable                     data = new DataTable();
	private final HashMap<String, Object> options;
	private final String                  id;
	private final String                  type;
	private String                        apiKey;
	
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
		bld.append(this.makeDataTable());
		bld.append("chart.draw(view, options); }");
		
		return bld.toString();
	}

	/**
	 * phase 1
	 *
	 * @return
	 */
	private String makeFunction()
	{
		final StringBuilder bld = new StringBuilder();
		if(this.type.equalsIgnoreCase("gantt") || this.type.equalsIgnoreCase("orgchart")
			|| this.type.equalsIgnoreCase("timeline"))
		{
			bld.append(
				"google.charts.load('visualization', 'current', {'packages': ['" + this.type.toLowerCase() + "']}); ");
		}
		else if(this.type.equalsIgnoreCase("geochart") || this.type.equalsIgnoreCase("map"))
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
	private String makeOptions()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var options ={ ");

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
	private String makeDataTable()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var data = new google.visualization.DataTable(); ");
		
		for(final Column col : this.data.getColumns())
		{
			bld.append("data.addColumn(" + col.jsPrint() + "); ");
		}
		
		bld.append("data.addRows([");
		for(final Row row : this.data.getRows())
		{
			bld.append("[");
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
		bld.append("var view = new google.visualization.DataView(data); ");
		return bld.toString();
	}
}

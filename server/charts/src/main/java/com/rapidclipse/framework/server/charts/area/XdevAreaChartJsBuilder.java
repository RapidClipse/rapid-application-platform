
package com.rapidclipse.framework.server.charts.area;

import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Row;
import com.rapidclipse.framework.server.charts.Value;


public class XdevAreaChartJsBuilder
{
	private final AreaChartComponentState areaState;
	private final String                  id;

	public XdevAreaChartJsBuilder(final AreaChartComponentState areaState, final String id)
	{
		super();
		this.areaState = areaState;
		this.id        = id;
	}
	
	/*
	 * Makes a String for executeJs. It will construct a JavaScript side with all the Attributes the Chart needs
	 * Why this way? Simple there is no shared storage for JS and Java anymore like in V8 and Polymer is to complex for
	 * a simple solution
	 * If Google changes anything on the syntax, this methods are to change.
	 */
	// TODO: Listener or trigger?
	// - new model
	// - model changed?
	public String constructChart()
	{
		final StringBuilder bld = new StringBuilder();
		
		bld.append(this.makeFunction());
		bld.append(this.makeOptions());
		bld.append(this.makeDataTable());
		bld.append("chart.draw(view, options); }");
		
		return bld.toString();
	}

	private String makeFunction()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("google.charts.load('visualization', 'current', {'packages': ['corechart']}); ");
		bld.append("google.charts.setOnLoadCallback(drawChart); ");
		bld.append("function drawChart(){ ");
		bld.append("var chart = new google.visualization.AreaChart(document.getElementById('" + this.id + "')); ");
		
		return bld.toString();
	}

	private String makeOptions()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var options ={ ");
		
		bld.append("title: '" + this.areaState.getConfig().getTitle() + "', ");
		bld.append("titleTextStyle: " + this.areaState.getConfig().getTitleTextStyle() + ", ");
		bld.append("backgroundColor: " + this.areaState.getConfig().getBackgroundColor() + ", ");
		bld.append("fontName: '" + this.areaState.getConfig().getFontName() + "', ");
		bld.append("fontSize: " + this.areaState.getConfig().getFontSize() + ", ");
		bld.append("legend: " + this.areaState.getConfig().getLegend() + ", ");
		bld.append("colors: " + this.areaState.getConfig().getColorsString() + ", ");
		bld.append("pointSize: " + this.areaState.getConfig().getPointSize() + ", ");
		bld.append("pointShape: '" + this.areaState.getConfig().getPointShape() + "', ");
		bld.append("lineWidth: " + this.areaState.getConfig().getLineWidth() + ", ");
		bld.append("lineDashStyle: " + this.areaState.getConfig().getLineDashStyle() + ", ");
		bld.append("chartArea: " + this.areaState.getConfig().getChartArea() + ", ");
		bld.append("isStacked: " + this.areaState.getConfig().isStacked() + ", ");
		bld.append("hAxis: " + this.areaState.getConfig().gethAxis() + ", ");
		bld.append("vAxis: " + this.areaState.getConfig().getvAxis() + ", ");
		bld.append("tooltip:" + this.areaState.getConfig().getTooltip() + ", ");
		
		bld.append("}; ");
		
		return bld.toString();
	}
	
	private String makeDataTable()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var data = new google.visualization.DataTable(); ");
		
		for(final Column col : this.areaState.getDataTable().getColumns())
		{
			bld.append("data.addColumn(" + col.jsPrint() + "); ");
		}
		
		bld.append("data.addRows([");
		int i = 1;
		for(final Row row : this.areaState.getDataTable().getRows())
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
			if(i != this.areaState.getDataTable().getRows().size())
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

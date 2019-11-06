
package com.rapidclipse.framework.server.charts.pie;

import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Row;
import com.rapidclipse.framework.server.charts.Value;


public class XdevPieChartJsBuilder
{
	private final PieChartComponentState pieState;
	private final String                 id;
	
	public XdevPieChartJsBuilder(final PieChartComponentState pieState, final String id)
	{
		super();
		this.pieState = pieState;
		this.id       = id;
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
		bld.append("var chart = new google.visualization.PieChart(document.getElementById('" + this.id + "')); ");

		return bld.toString();
	}
	
	private String makeOptions()
	{
		final StringBuilder bld = new StringBuilder();
		bld.append("var options ={ ");

		// bld.append("width: 600, ");
		// bld.append("height: 300, ");
		bld.append("title: '" + this.pieState.getConfig().getTitle() + "', ");
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

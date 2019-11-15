
package com.rapidclipse.framework.server.charts.diff;

import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.config.IdGenerator;
import com.rapidclipse.framework.server.charts.config.Series;
import com.rapidclipse.framework.server.charts.config.XdevSeries;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;
import com.rapidclipse.framework.server.charts.scatter.XdevScatterChartConfig;
import com.rapidclipse.framework.server.charts.scatter.XdevScatterChartModel;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.page.Page;


@Tag("diff-Scatter-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevDiffScatterChart extends Composite<Div> implements HasSize
{
	private DataTable              oldData;
	private DataTable              newData;
	private Series                 series;
	private final String           id;
	private XdevScatterChartConfig config;
	
	public XdevDiffScatterChart()
	{
		super();
		this.id     = IdGenerator.generateId();
		this.config = new XdevScatterChartConfig();
		this.setId(this.id);
	}

	private DataTable setDataTable(final XdevChartModel model)
	{
		final XdevScatterChartModel chartModel = (XdevScatterChartModel)model;
		
		if(this.series == null)
		{
			for(final XdevSeries ser : chartModel.getSeries())
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
	
	public void setModel(final XdevChartModel oldModel, final XdevChartModel newModel)
	{
		this.oldData = this.setDataTable(oldModel);
		this.newData = this.setDataTable(newModel);
		this.buildChart();
	}

	public void setConfig(final XdevScatterChartConfig config)
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

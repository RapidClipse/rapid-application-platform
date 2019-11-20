
package com.rapidclipse.framework.server.charts.diff;

import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.column.XdevColumnChartConfig;
import com.rapidclipse.framework.server.charts.config.IdGenerator;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.page.Page;


@Tag("diff-Column-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevDiffColumnChart extends Composite<Div> implements HasSize
{
	private DataTable             oldData;
	private DataTable             newData;
	private final String          id;
	private XdevColumnChartConfig config;

	public XdevDiffColumnChart()
	{
		super();
		this.id     = IdGenerator.generateId();
		this.config = new XdevColumnChartConfig();
		this.setId(this.id);
	}
	
	private DataTable setDataTable(final XdevChartModel model)
	{
		Row.createFromHashmap(model.getData()).forEach(row -> model.getDataTable().getRows().add(row));

		return model.getDataTable();
		
	}

	public void setModel(final XdevChartModel oldModel, final XdevChartModel newModel)
	{
		this.oldData = this.setDataTable(oldModel);
		this.newData = this.setDataTable(newModel);
		this.buildChart();
	}
	
	public void setConfig(final XdevColumnChartConfig config)
	{
		this.config = config;
		
	}
	
	public void buildChart()
	{
		final ChartJsBuilder oldBuild = new ChartJsBuilder(this.oldData,
			this.config.getOptions(), this.id, "ColumnChart");
		final ChartJsBuilder newBuild = new ChartJsBuilder(this.newData,
			this.config.getOptions(), this.id, "ColumnChart");
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

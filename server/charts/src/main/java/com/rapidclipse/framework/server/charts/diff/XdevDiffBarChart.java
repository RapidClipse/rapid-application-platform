
package com.rapidclipse.framework.server.charts.diff;

import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.bar.XdevBarChartConfig;
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


@Tag("diff-bar-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevDiffBarChart extends Composite<Div> implements HasSize
{
	private DataTable          oldData;
	private DataTable          newData;
	private final String       id;
	private XdevBarChartConfig config;

	public XdevDiffBarChart()
	{
		super();
		this.id     = IdGenerator.generateId();
		this.config = new XdevBarChartConfig();
		this.setId(this.id);
	}
	
	private DataTable setDataTable(final XdevChartModel model)
	{
		final DataTable table = new DataTable();
		model.getDataTable().getColumns().forEach(column -> table.getColumns().add(column));
		Row.createFromHashmap(model.getData()).forEach(row -> table.getRows().add(row));
		return table;
		
	}

	public void setModel(final XdevChartModel oldModel, final XdevChartModel newModel)
	{
		this.oldData = this.setDataTable(oldModel);
		this.newData = this.setDataTable(newModel);
		this.buildChart();
	}
	
	public void setConfig(final XdevBarChartConfig config)
	{
		this.config = config;
		
	}
	
	public void buildChart()
	{
		final ChartJsBuilder oldBuild = new ChartJsBuilder(this.oldData,
			this.config.getOptions(), this.id, "BarChart");
		final ChartJsBuilder newBuild = new ChartJsBuilder(this.newData,
			this.config.getOptions(), this.id, "BarChart");
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

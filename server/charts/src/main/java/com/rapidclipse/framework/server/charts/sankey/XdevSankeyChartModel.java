
package com.rapidclipse.framework.server.charts.sankey;

import java.util.LinkedHashMap;

import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;


public class XdevSankeyChartModel implements XdevChartModel
{
	private DataTable                                                  dataTable = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data      = new LinkedHashMap<>();

	public XdevSankeyChartModel()
	{
		this.getDataTable().getColumns().add(Column.create("from", "from", ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create("to", "to", ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create("weight", "weight", ColumnType.NUMBER));
	}

	@Override
	public DataTable getDataTable()
	{
		if(this.dataTable == null)
		{
			this.dataTable = new DataTable();
		}
		return this.dataTable;
	}
	
	@Override
	public LinkedHashMap<Object, LinkedHashMap<String, Object>> getData()
	{
		return this.data;
	}
	
	public void addItem(final String from, final String to, final int weight)
	{
		this.getDataTable().getRows().add(Row.create(from, to, weight));
	}
}

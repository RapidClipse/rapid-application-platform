
package com.rapidclipse.framework.server.charts.histogram;

import java.util.LinkedHashMap;

import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;


public class XdevHistogramChartModel implements XdevChartModel
{
	private DataTable                                                  dataTable = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data      = new LinkedHashMap<>();
	
	public XdevHistogramChartModel(final String valueName)
	{
		this.getDataTable().getColumns().add(Column.create("name", "name", ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create(valueName, valueName, ColumnType.NUMBER));
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

	public void addItem(final String name, final double value)
	{
		this.getDataTable().getRows().add(Row.create(name, value));
	}

	public void addItem(final String name, final int value)
	{
		this.getDataTable().getRows().add(Row.create(name, value));
	}
	
}


package com.rapidclipse.framework.server.charts.calendar;

import java.time.LocalDate;
import java.util.LinkedHashMap;

import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;


public class XdevCalendarChartModel implements XdevChartModel
{
	private DataTable                                                  dataTable = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data      = new LinkedHashMap<>();
	
	public XdevCalendarChartModel()
	{
		this.getDataTable().getColumns().add(Column.create("date", "date", ColumnType.DATE));
		this.getDataTable().getColumns().add(Column.create("value", "value", ColumnType.NUMBER));
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

	public void addItem(final LocalDate date, final int value)
	{
		this.getDataTable().getRows().add(Row.create(date, value));
	}
	
}

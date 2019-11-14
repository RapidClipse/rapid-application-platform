
package com.rapidclipse.framework.server.charts.scatter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.config.XdevSeries;
import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.MultiValue;
import com.rapidclipse.framework.server.charts.data.Row;
import com.rapidclipse.framework.server.charts.data.Value;


public class XdevScatterChartModel implements XdevChartModel
{
	private DataTable                                                  dataTable  = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data       = new LinkedHashMap<>();
	private final List<XdevSeries>                                     seriesList = new ArrayList<>();

	public XdevScatterChartModel(final String xcap, final ColumnType xTyp)
	{
		this.getDataTable().getColumns()
			.add(Column.create(xcap, xcap, xTyp));
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
	
	public List<XdevSeries> getSeries()
	{
		return this.seriesList;
	}

	@Override
	public LinkedHashMap<Object, LinkedHashMap<String, Object>> getData()
	{
		return this.data;
	}
	
	public void addCategory(final String cap, final ColumnType typ, final XdevScatterChartSeries series)
	{
		this.getDataTable().getColumns()
			.add(Column.create(cap, cap, typ));
		if(series != null)
		{
			this.seriesList.add(series);
		}
		else
		{
			this.seriesList.add(null);
		}
	}

	public void addItem(final Object... points)
	{
		final List<Value> values = new ArrayList<>();
		for(final Object o : points)
		{
			values.add(new Value(o));
		}
		final MultiValue mv = new MultiValue();
		mv.setV(values);
		this.getDataTable().getRows().add(Row.create(mv));
	}
	
}

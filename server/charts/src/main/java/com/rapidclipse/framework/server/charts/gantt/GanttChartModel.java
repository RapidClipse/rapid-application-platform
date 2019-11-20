/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts.gantt;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.MultiValue;
import com.rapidclipse.framework.server.charts.data.Row;
import com.rapidclipse.framework.server.charts.data.Value;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class GanttChartModel implements ChartModel
{
	private DataTable                                                  dataTable = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data      = new LinkedHashMap<>();

	/**
	 * Constructor for Gantt.
	 *
	 * @param id
	 * @param name
	 * @param start
	 * @param end
	 * @param duration
	 *            in milliseconds
	 * @param precent
	 *            0-100
	 * @param dependencie
	 *            name of precursor
	 */
	public GanttChartModel(
		final String id,
		final String name,
		final String start,
		final String end,
		final String duration,
		final String precent,
		final String dependencie)
	{
		this.getDataTable().getColumns().add(Column.create(id, id, ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create(name, name, ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create(start, start, ColumnType.DATE));
		this.getDataTable().getColumns().add(Column.create(end, end, ColumnType.DATE));
		this.getDataTable().getColumns().add(Column.create(duration, duration, ColumnType.NUMBER));
		this.getDataTable().getColumns().add(Column.create(precent, precent, ColumnType.NUMBER));
		this.getDataTable().getColumns().add(Column.create(dependencie, dependencie, ColumnType.STRING));
		
	}
	
	/**
	 * Constructor for Gantt with optional resource id.
	 *
	 * @param id
	 * @param name
	 * @param resource
	 * @param start
	 * @param end
	 * @param duration
	 *            in milliseconds
	 * @param precent
	 *            0-100
	 * @param dependencie
	 *            name of precursor
	 */
	public GanttChartModel(
		final String id,
		final String name,
		final String resource,
		final String start,
		final String end,
		final String duration,
		final String precent,
		final String dependencie)
	{
		this.getDataTable().getColumns().add(Column.create(id, id, ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create(name, name, ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create(resource, resource, ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create(start, start, ColumnType.DATE));
		this.getDataTable().getColumns().add(Column.create(end, end, ColumnType.DATE));
		this.getDataTable().getColumns().add(Column.create(duration, duration, ColumnType.NUMBER));
		this.getDataTable().getColumns().add(Column.create(precent, precent, ColumnType.NUMBER));
		this.getDataTable().getColumns().add(Column.create(dependencie, dependencie, ColumnType.STRING));
		
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

	public void addItem(final String id, final String name, final Object... v)
	{
		final List<Value> values = new ArrayList<>();
		values.add(new Value(id));
		values.add(new Value(name));
		
		for(final Object o : v)
		{
			values.add(new Value(o));
		}
		
		final MultiValue mv = new MultiValue();
		mv.setV(values);
		this.dataTable.getRows().add(Row.create(mv));
	}
	
}

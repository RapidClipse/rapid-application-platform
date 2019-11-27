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

package com.rapidclipse.framework.server.charts.scatter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.ColumnType;
import com.rapidclipse.framework.server.charts.config.Line;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.MultiValue;
import com.rapidclipse.framework.server.charts.data.Row;
import com.rapidclipse.framework.server.charts.data.Value;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class ScatterChartModel implements ChartModel
{
	private DataTable                                                  dataTable  = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data       = new LinkedHashMap<>();
	private final List<Line>                                     seriesList = new ArrayList<>();
	
	public ScatterChartModel(final String xcap, final ColumnType xTyp)
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

	public List<Line> getSeries()
	{
		return this.seriesList;
	}
	
	@Override
	public LinkedHashMap<Object, LinkedHashMap<String, Object>> getData()
	{
		return this.data;
	}

	public void addCategory(final String cap, final ColumnType typ, final ScatterChartSeries series)
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
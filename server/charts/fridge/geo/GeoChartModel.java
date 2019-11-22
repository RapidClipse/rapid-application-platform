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
package com.rapidclipse.framework.server.charts.geo;

import java.util.LinkedHashMap;

import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class GeoChartModel implements ChartModel
{
	
	private DataTable                                                  dataTable = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data      = new LinkedHashMap<>();
	
	public GeoChartModel(final String caption, final String value)
	{
		this.getDataTable().getColumns()
			.add(Column.create(caption.toLowerCase(), caption, ColumnType.STRING));
		this.getDataTable().getColumns()
			.add(Column.create(value.toLowerCase(), value, ColumnType.NUMBER));
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
	
	public void addItem(final String country, final Object value)
	{
		this.getDataTable().getRows().add(Row.create(country, value));
	}
	
}

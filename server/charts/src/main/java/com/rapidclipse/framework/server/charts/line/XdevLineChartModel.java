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
package com.rapidclipse.framework.server.charts.line;

import java.time.LocalDate;
import java.util.LinkedHashMap;

import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataRoleType;
import com.rapidclipse.framework.server.charts.data.DataTable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevLineChartModel implements XdevChartModel
{
	
	private DataTable                                                  dataTable  = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data       = new LinkedHashMap<>();
	private final LinkedHashMap<String, Object>                        categories = new LinkedHashMap<>();
	
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
	
	public void addYCategory(final String caption)
	{
		this.categories.put(caption, null);
		this.getDataTable().getColumns()
			.add(Column.create(caption.toLowerCase(), caption, ColumnType.NUMBER));
	}
	
	public void addXCategory(final String caption, final ColumnType type)
	{
		this.getDataTable().getColumns().add(0, Column.create(caption.toLowerCase(), caption, type));
	}
	
	public void addHiddenCategory(final String caption, final ColumnType type)
	{
		this.getDataTable().getColumns().add(Column.create(caption.toLowerCase(), "hidden", type));
	}

	public void addIntervalCategory(final String caption)
	{
		this.categories.put(caption, null);
		this.getDataTable().getColumns()
			.add(Column.dataRoleColumn(caption.toLowerCase(), caption, ColumnType.NUMBER, DataRoleType.INTERVALE));
	}
	
	@SuppressWarnings("unchecked")
	public void addItem(final String category, Object xValue, final Integer yValue)
	{
		if(xValue instanceof LocalDate)
		{
			final LocalDate date = (LocalDate)xValue;
			
			xValue = "Date(" + date.getYear() + ", " + date.getMonthValue() + ", "
				+ date.getDayOfMonth() + ")";
		}
		
		if(!this.data.containsKey(xValue))
		{
			final LinkedHashMap<String, Object> rowData = (LinkedHashMap<String, Object>)this.categories
				.clone();
			rowData.put(category, yValue);
			this.data.put(xValue, rowData);
		}
		else
		{
			final LinkedHashMap<String, Object> rowData = this.data.get(xValue);
			rowData.put(category, yValue);
			this.data.put(xValue, rowData);
		}

	}

}

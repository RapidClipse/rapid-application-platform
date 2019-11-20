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
package com.rapidclipse.framework.server.charts.timeline;

import java.util.LinkedHashMap;

import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;
import com.rapidclipse.framework.server.charts.data.Row;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevTimeLineChartModel implements XdevChartModel
{

	private DataTable                                                  dataTable = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data      = new LinkedHashMap<>();
	
	public XdevTimeLineChartModel()
	{
		this.getDataTable().getColumns()
			.add(Column.create("category", "category", ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create("caption", "caption", ColumnType.STRING));
		this.getDataTable().getColumns().add(Column.create("start", "start", ColumnType.DATE));
		this.getDataTable().getColumns().add(Column.create("end", "end", ColumnType.DATE));

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
	
	/**
	 *
	 * @param category
	 * @param caption
	 * @param start
	 *            LocalDate or LocalTime
	 * @param end
	 *            LocalDate or LocalTime
	 */
	public void addItem(
		final String category,
		final String caption,
		final Object start,
		final Object end)
	{
		this.getDataTable().getRows().add(Row.create(category, caption, start, end));
	}
	
}

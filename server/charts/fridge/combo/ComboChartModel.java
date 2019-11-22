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
package com.rapidclipse.framework.server.charts.combo;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.ColumnType;
import com.rapidclipse.framework.server.charts.config.Line;
import com.rapidclipse.framework.server.charts.data.DataTable;


/**
 * @author XDEV Software
 * @since 10.02.00
 */
public class ComboChartModel implements ChartModel
{
	private DataTable                                                  dataTable  = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data       = new LinkedHashMap<>();
	private final LinkedHashMap<String, Object>                        categories = new LinkedHashMap<>();
	private final List<Line>                                     seriesList = new ArrayList<>();
	
	public ComboChartModel()
	{
		this.getDataTable().getColumns()
			.add(Column.create("ycaption", "ycaption", ColumnType.STRING));
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
	
	public List<Line> getSeries()
	{
		return this.seriesList;
	}
	
	public void addCategory(final String caption, final ComboChartSeries series)
	{
		this.categories.put(caption, null);
		this.getDataTable().getColumns()
			.add(Column.create(caption.toLowerCase(), caption, ColumnType.NUMBER));
		
		if(series != null)
		{
			this.seriesList.add(series);
		}
		else
		{
			this.seriesList.add(null);
		}
	}
	
	@SuppressWarnings("unchecked")
	public void addItem(final String group, final String category, final Object value)
	{
		if(!this.data.containsKey(group))
		{
			final LinkedHashMap<String, Object> v = (LinkedHashMap<String, Object>)this.categories
				.clone();
			v.put(category, value);
			this.data.put(group, v);
		}
		else
		{
			final LinkedHashMap<String, Object> v = this.data.get(group);
			v.put(category, value);
			this.data.put(group, v);
		}
	}

}

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
package com.rapidclipse.framework.server.charts.area;

import java.util.LinkedHashMap;

import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class AreaChartModel implements ChartModel
{

	private DataTable                                                  dataTable  = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data       = new LinkedHashMap<>();
	private final LinkedHashMap<String, Object>                        categories = new LinkedHashMap<>();

	/**
	 *
	 * @param typ
	 *            String, Date, Number, Datetime
	 */
	public AreaChartModel(final ColumnType typ)
	{
		this.getDataTable().getColumns()
			.add(Column.create("xcaption", "xcaption", typ));
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
	 * Adds a new category to the XdevAreaChartModel.<br>
	 *
	 * @param category
	 */
	public void addCategory(final String category)
	{
		this.categories.put(category, null);
		this.getDataTable().getColumns().add(Column.create(category, category, ColumnType.NUMBER));
	}

	public void addItem(final Object group, final String category, final Integer value)
	{
		this.addItemInternal(group, category, value);
	}

	public void addItem(final Object group, final String category, final Double value)
	{
		this.addItemInternal(group, category, value);
	}

	@SuppressWarnings("unchecked")
	private void addItemInternal(final Object group, final String category, final Object value)
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

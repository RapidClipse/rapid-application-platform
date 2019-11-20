/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.charts.bar;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;

import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.data.Column;
import com.rapidclipse.framework.server.charts.data.ColumnType;
import com.rapidclipse.framework.server.charts.data.DataTable;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class XdevBarChartModel implements XdevChartModel
{

	private DataTable                                                  dataTable  = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data       = new LinkedHashMap<>();
	private final LinkedHashMap<String, Object>                        categories = new LinkedHashMap<>();

	public XdevBarChartModel()
	{
		this.getDataTable().getColumns()
			.add(Column.create("ycaption", "ycaption", ColumnType.STRING));
	}

	@Override
	public LinkedHashMap<Object, LinkedHashMap<String, Object>> getData()
	{
		return this.data;
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

	/**
	 * Creates a new category with the given caption. <br>
	 *
	 * @param caption
	 */
	public void addCategory(final String caption)
	{
		this.categories.put(caption, null);

		final List<Column> columns = this.getDataTable().getColumns();

		if(columns.isEmpty())
		{
			final Column valueColumn = Column.create(caption, caption, ColumnType.NUMBER);

			this.getDataTable().getColumns().add(valueColumn);
			
		}
		else
		{
			final Optional<Column> item = columns.stream()
				.filter(column -> column.getLabel().equals(caption)).findFirst();

			if(!item.isPresent())
			{
				final Column valueColumn = Column.create(caption, caption, ColumnType.NUMBER);
				
				this.getDataTable().getColumns().add(valueColumn);
				
			}
		}

	}

	public void addHiddenCategory(final String caption)
	{
		this.getDataTable().getColumns()
			.add(Column.create(caption.toLowerCase(), "hidden", ColumnType.NUMBER));
	}

	/**
	 * Adds a new item to the model. <br>
	 *
	 * @param group
	 * @param category
	 * @param value
	 */
	public void addItem(final String group, final String category, final Integer value)
	{
		this.addItemInternal(group, category, value);
	}

	/**
	 * Adds a new item to the model. <br>
	 *
	 * @param group
	 * @param category
	 * @param value
	 */
	public void addItem(final String group, final String category, final Double value)
	{
		this.addItemInternal(group, category, value);
	}

	@SuppressWarnings("unchecked")
	private void addItemInternal(
		final String group,
		final String category,
		final Object value)
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

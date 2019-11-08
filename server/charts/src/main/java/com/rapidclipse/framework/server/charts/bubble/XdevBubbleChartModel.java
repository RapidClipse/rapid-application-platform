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

package com.rapidclipse.framework.server.charts.bubble;

import java.util.LinkedHashMap;

import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.ColumnType;
import com.rapidclipse.framework.server.charts.DataTable;
import com.rapidclipse.framework.server.charts.Row;
import com.rapidclipse.framework.server.charts.XdevChartModel;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class XdevBubbleChartModel implements XdevChartModel
{
	
	private DataTable                                                  dataTable = null;
	private final LinkedHashMap<Object, LinkedHashMap<String, Object>> data      = new LinkedHashMap<>();
	private boolean                                                    groups    = false;

	/**
	 * With group caption
	 *
	 * @param xColumnCaption
	 * @param yColumnCaption
	 * @param groupCaption
	 * @param sizeCaption
	 */
	public XdevBubbleChartModel(
		final String xColumnCaption,
		final String yColumnCaption,
		final String groupCaption,
		final String sizeCaption)
	{
		this.groups = true;
		this.getDataTable().getColumns().add(Column.create("caption", "Caption", ColumnType.STRING));
		this.getDataTable().getColumns()
			.add(Column.create(xColumnCaption.toLowerCase(), xColumnCaption, ColumnType.NUMBER));
		this.getDataTable().getColumns()
			.add(Column.create(yColumnCaption.toLowerCase(), yColumnCaption, ColumnType.NUMBER));
		this.getDataTable().getColumns()
			.add(Column.create(groupCaption.toLowerCase(), groupCaption, ColumnType.STRING));
		this.getDataTable().getColumns()
			.add(Column.create(sizeCaption.toLowerCase(), sizeCaption, ColumnType.NUMBER));
	}
	
	/**
	 * Without group caption.
	 * colorAxis needs this one.
	 *
	 * @param xColumnCaption
	 * @param yColumnCaption
	 * @param sizeCaption
	 */
	public XdevBubbleChartModel(
		final String xColumnCaption,
		final String yColumnCaption,
		final String sizeCaption)
	{
		this.getDataTable().getColumns().add(Column.create("caption", "Caption", ColumnType.STRING));
		this.getDataTable().getColumns()
			.add(Column.create(xColumnCaption.toLowerCase(), xColumnCaption, ColumnType.NUMBER));
		this.getDataTable().getColumns()
			.add(Column.create(yColumnCaption.toLowerCase(), yColumnCaption, ColumnType.NUMBER));
		this.getDataTable().getColumns()
			.add(Column.create(sizeCaption.toLowerCase(), sizeCaption, ColumnType.NUMBER));
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
	 * addItem for Bubblecharts with group captions.
	 * If you called the constructor without groups use the addItem method without group parameter
	 *
	 * @param caption
	 * @param x
	 * @param y
	 * @param group
	 * @param size
	 * @throws Exception
	 *             if the wrong constructor get called it will throw a IllegalStateException
	 */
	public void addItem(
		final String caption,
		final Double x,
		final Double y,
		final String group,
		final double size)
	{
		if(!this.groups)
		{
			throw new IllegalStateException("Wrong addItem method used");
		}
		this.getDataTable().getRows().add(Row.create(caption, x, y, group, size));
	}

	/**
	 * addItem for Bubblecharts without group caption
	 *
	 * @param caption
	 * @param x
	 * @param y
	 * @param size
	 * @throws Exception
	 *             if the wrong constructor get called it will throw a IllegalStateException
	 */
	public void addItem(
		final String caption,
		final Double x,
		final Double y,
		final double size)
	{
		if(this.groups)
		{
			throw new IllegalStateException("Wrong addItem method used");
		}
		this.getDataTable().getRows().add(Row.create(caption, x, y, size));
	}
}

/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.table;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsHtml;
import com.rapidclipse.framework.server.charts.Cell;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Column.Type;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("table-chart")
public class TableChart extends AbstractChart
	implements AllowsHtml, HasChartSize
{
	public TableChart()
	{
		super("Table", "table");
	}

	public boolean getAlternatingRowStyle()
	{
		return properties().get("alternatingRowStyle", true);
	}

	public void setAlternatingRowStyle(final boolean alternatingRowStyle)
	{
		properties().put("alternatingRowStyle", alternatingRowStyle);
	}

	public CssClassNames getCssClassNames()
	{
		return properties().get("cssClassNames", null);
	}

	public void setCssClassNames(final CssClassNames cssClassNames)
	{
		properties().put("cssClassNames", cssClassNames);
	}

	public int getFirstRowNumber()
	{
		return properties().get("firstRowNumber", 1);
	}

	public void setFirstRowNumber(final int firstRowNumber)
	{
		properties().put("firstRowNumber", firstRowNumber);
	}

	public Integer getFrozenColumns()
	{
		return properties().get("frozenColumns", null);
	}

	public void setFrozenColumns(final Integer frozenColumns)
	{
		properties().put("frozenColumns", frozenColumns);
	}

	public ActionMode getPage()
	{
		return properties().get("page", ActionMode.DISABLE);
	}

	public void setPage(final ActionMode page)
	{
		properties().put("page", page);
	}

	public int getPageSize()
	{
		return properties().get("pageSize", 10);
	}

	public void setPageSize(final int pageSize)
	{
		properties().put("pageSize", pageSize);
	}

	public PagingButtons getPagingButtons()
	{
		return properties().get("pagingButtons", null);
	}

	public void setPagingButtons(final PagingButtons pagingButtons)
	{
		properties().put("pagingButtons", pagingButtons);
	}

	public boolean getRtlTable()
	{
		return properties().get("rtlTable", false);
	}

	public void setRtlTable(final boolean rtlTable)
	{
		properties().put("rtlTable", rtlTable);
	}

	public int getScrollLeftStartPosition()
	{
		return properties().get("scrollLeftStartPosition", 0);
	}

	public void setScrollLeftStartPosition(final int scrollLeftStartPosition)
	{
		properties().put("scrollLeftStartPosition", scrollLeftStartPosition);
	}

	public boolean getShowRowNumber()
	{
		return properties().get("showRowNumber", false);
	}

	public void setShowRowNumber(final boolean showRowNumber)
	{
		properties().put("showRowNumber", showRowNumber);
	}

	public ActionMode getSort()
	{
		return properties().get("sort", ActionMode.ENABLE);
	}

	public void setSort(final ActionMode sort)
	{
		properties().put("sort", sort);
	}

	public boolean getSortAscending()
	{
		return properties().get("sortAscending", true);
	}

	public void setSortAscending(final boolean sortAscending)
	{
		properties().put("sortAscending", sortAscending);
	}

	public int getSortColumn()
	{
		return properties().get("sortColumn", -1);
	}

	public void setSortColumn(final int sortColumn)
	{
		properties().put("sortColumn", sortColumn);
	}

	public int getStartPage()
	{
		return properties().get("startPage", 0);
	}

	public void setStartPage(final int startPage)
	{
		properties().put("startPage", startPage);
	}

	@Override
	public void showSampleData()
	{
		getModel().removeAll()
			.addColumn(Column.New(Type.STRING, "Name"))
			.addColumn(Column.New(Type.NUMBER, "Salary"))
			.addColumn(Column.New(Type.BOOLEAN, "Full Time Employee"))
			.addRow("Mike", Cell.New(10000, "$10.000"), true)
			.addRow("Jim", Cell.New(8000, "$8.000"), false)
			.addRow("Alice", Cell.New(12500, "$12.500"), true)
			.addRow("Bob", Cell.New(7000, "$7.000"), true);
	}
}

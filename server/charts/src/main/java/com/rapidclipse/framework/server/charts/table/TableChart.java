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

package com.rapidclipse.framework.server.charts.table;

import com.rapidclipse.framework.server.charts.AllowsHtml;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("table-chart")
public class TableChart extends ChartBase
	implements AllowsHtml, HasChartSize
{
	public TableChart()
	{
		super("Table", "table");
	}

	public Boolean getAlternatingRowStyle()
	{
		return properties().get("alternatingRowStyle");
	}
	
	public void setAlternatingRowStyle(final Boolean alternatingRowStyle)
	{
		properties().put("alternatingRowStyle", alternatingRowStyle);
	}
	
	public CssClassNames getCssClassNames()
	{
		return properties().get("cssClassNames");
	}
	
	public void setCssClassNames(final CssClassNames cssClassNames)
	{
		properties().put("cssClassNames", cssClassNames);
	}
	
	public Integer getFirstRowNumber()
	{
		return properties().get("firstRowNumber");
	}
	
	public void setFirstRowNumber(final Integer firstRowNumber)
	{
		properties().put("firstRowNumber", firstRowNumber);
	}
	
	public Integer getFrozenColumns()
	{
		return properties().get("frozenColumns");
	}
	
	public void setFrozenColumns(final Integer frozenColumns)
	{
		properties().put("frozenColumns", frozenColumns);
	}
	
	public ActionMode getPage()
	{
		return properties().get("page");
	}
	
	public void setPage(final ActionMode page)
	{
		properties().put("page", page);
	}
	
	public Integer getPageSize()
	{
		return properties().get("pageSize");
	}
	
	public void setPageSize(final Integer pageSize)
	{
		properties().put("pageSize", pageSize);
	}
	
	public PagingButtons getPagingButtons()
	{
		return properties().get("pagingButtons");
	}
	
	public void setPagingButtons(final PagingButtons pagingButtons)
	{
		properties().put("pagingButtons", pagingButtons);
	}
	
	public Boolean getRtlTable()
	{
		return properties().get("rtlTable");
	}
	
	public void setRtlTable(final Boolean rtlTable)
	{
		properties().put("rtlTable", rtlTable);
	}
	
	public Integer getScrollLeftStartPosition()
	{
		return properties().get("scrollLeftStartPosition");
	}
	
	public void setScrollLeftStartPosition(final Integer scrollLeftStartPosition)
	{
		properties().put("scrollLeftStartPosition", scrollLeftStartPosition);
	}
	
	public Boolean getShowRowNumber()
	{
		return properties().get("showRowNumber");
	}
	
	public void setShowRowNumber(final Boolean showRowNumber)
	{
		properties().put("showRowNumber", showRowNumber);
	}
	
	public ActionMode getSort()
	{
		return properties().get("sort");
	}
	
	public void setSort(final ActionMode sort)
	{
		properties().put("sort", sort);
	}
	
	public Boolean getSortAscending()
	{
		return properties().get("sortAscending");
	}
	
	public void setSortAscending(final Boolean sortAscending)
	{
		properties().put("sortAscending", sortAscending);
	}
	
	public Integer getSortColumn()
	{
		return properties().get("sortColumn");
	}
	
	public void setSortColumn(final Integer sortColumn)
	{
		properties().put("sortColumn", sortColumn);
	}
	
	public Integer getStartPage()
	{
		return properties().get("startPage");
	}
	
	public void setStartPage(final Integer startPage)
	{
		properties().put("startPage", startPage);
	}
}

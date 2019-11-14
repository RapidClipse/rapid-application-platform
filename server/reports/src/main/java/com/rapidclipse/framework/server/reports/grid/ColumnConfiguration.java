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

package com.rapidclipse.framework.server.reports.grid;

import java.util.Optional;

import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.Grid.Column;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public class ColumnConfiguration<T>
{
	private final Column<T> gridColumn;
	private boolean         visible = true;
	private String          header;
	private ColumnTextAlign columnAlignment;
	private Integer         columnWidth;
	
	ColumnConfiguration(final Column<T> gridColumn)
	{
		super();
		
		this.gridColumn      = gridColumn;
		this.header          = Optional.ofNullable(gridColumn.getKey()).orElse("");
		this.columnAlignment = gridColumn.getTextAlign();
	}
	
	Column<T> getGridColumn()
	{
		return this.gridColumn;
	}
	
	public boolean isVisible()
	{
		return this.visible;
	}
	
	public ColumnConfiguration<T> setVisible(final boolean visible)
	{
		this.visible = visible;
		return this;
	}
	
	public String getHeader()
	{
		return this.header;
	}
	
	public ColumnConfiguration<T> setHeader(final String header)
	{
		this.header = header;
		return this;
	}
	
	public ColumnTextAlign getColumnAlignment()
	{
		return this.columnAlignment;
	}
	
	public ColumnConfiguration<T> setColumnAlignment(final ColumnTextAlign columnAlignment)
	{
		this.columnAlignment = columnAlignment;
		return this;
	}
	
	public Integer getColumnWidth()
	{
		return this.columnWidth;
	}
	
	public ColumnConfiguration<T> setColumnWidth(final Integer columnWidth)
	{
		this.columnWidth = columnWidth;
		return this;
	}
}

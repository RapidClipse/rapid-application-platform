/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.reports.grid.column;

import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.function.SerializableFunction;


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
	
	ColumnConfiguration(
		final Column<T> gridColumn,
		final SerializableFunction<Column<?>, String> headerResolver)
	{
		this.gridColumn      = gridColumn;
		this.header          = headerResolver.apply(gridColumn);
		this.columnAlignment = gridColumn.getTextAlign();
	}
	
	public Column<T> getGridColumn()
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

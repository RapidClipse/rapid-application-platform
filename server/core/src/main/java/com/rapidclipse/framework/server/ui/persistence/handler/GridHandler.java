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

package com.rapidclipse.framework.server.ui.persistence.handler;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.data.provider.SortDirection;


@SuppressWarnings("rawtypes")
public class GridHandler extends ComponentHandler<Grid>
{
	protected static final String COLUMN_SORT_ORDER = "columnSortOrder";
	protected static final String COLUMN_WIDTH      = "columnWidth";
	
	public GridHandler()
	{
		super();
	}

	@Override
	public Class<Grid> handledType()
	{
		return Grid.class;
	}

	@Override
	protected void addEntryValues(final Map<String, Object> entryValues, final Grid grid)
	{
		super.addEntryValues(entryValues, grid);

		storeColumnSortOrder(entryValues, grid);
		storeColumnWidth(entryValues, grid);
	}

	@SuppressWarnings("unchecked")
	protected void storeColumnWidth(final Map<String, Object> entryValues, final Grid grid)
	{
		final List<Column>        columns  = grid.getColumns();
		final Map<String, String> widthMap = columns.stream()
			.filter(c -> c.getKey() != null && c.getWidth() != null)
			.collect(Collectors.toMap(Column::getKey, Column::getWidth));
		if(widthMap.size() > 0)
		{
			entryValues.put(COLUMN_WIDTH, widthMap);
		}
	}

	@SuppressWarnings("unchecked")
	protected void storeColumnSortOrder(final Map<String, Object> entryValues, final Grid grid)
	{
		final List<GridSortOrder> sortOrder = grid.getSortOrder();
		final Map<String, String> sortMap   = sortOrder.stream()
			.filter(so -> so.getSorted().getKey() != null)
			.collect(Collectors.toMap(so -> so.getSorted().getKey(), so -> so.getDirection().name()));
		if(sortMap.size() > 0)
		{
			entryValues.put(COLUMN_SORT_ORDER, sortMap);
		}
	}

	@Override
	public void restore(final Grid grid, final GuiPersistenceEntry entry)
	{
		super.restore(grid, entry);

		restoreColumnSortOrder(grid, entry);
		restoreColumnWidth(grid, entry);
	}
	
	@SuppressWarnings("unchecked")
	protected void restoreColumnWidth(final Grid grid, final GuiPersistenceEntry entry)
	{
		final Map<String, String> widthMap = (Map<String, String>)entry.value(COLUMN_WIDTH);
		if(widthMap != null)
		{
			widthMap.entrySet().forEach(e -> {
				final Column column = grid.getColumnByKey(e.getKey());
				if(column != null)
				{
					column.setWidth(e.getValue());
				}
			});
		}
	}
	
	@SuppressWarnings("unchecked")
	protected void restoreColumnSortOrder(final Grid grid, final GuiPersistenceEntry entry)
	{
		final Map<String, String> sortMap = (Map<String, String>)entry.value(COLUMN_SORT_ORDER);
		if(sortMap != null)
		{
			final List<GridSortOrder> sortOrder = sortMap.entrySet().stream()
				.map(e -> {
					final Column column = grid.getColumnByKey(e.getKey());
					return column != null
						? new GridSortOrder<>(column, SortDirection.valueOf(e.getValue()))
						: null;
				})
				.filter(Objects::nonNull)
				.collect(Collectors.toList());
			grid.sort(sortOrder);
		}
	}
}

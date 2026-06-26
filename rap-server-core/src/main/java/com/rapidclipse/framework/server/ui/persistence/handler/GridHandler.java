/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.persistence.handler;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.ui.persistence.GuiPersistenceEntry;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.data.provider.SortDirection;


/**
 *
 * @author XDEV Software
 * @since 10.02.02
 */
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
	private void storeColumnWidth(final Map<String, Object> entryValues, final Grid grid)
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
	private void storeColumnSortOrder(final Map<String, Object> entryValues, final Grid grid)
	{
		final List<GridSortOrder>       sortOrder          = grid.getSortOrder();
		final List<Map<String, String>> persistedSortOrder = sortOrder.stream()
			.filter(so -> so.getSorted().getKey() != null)
			.map(this::sortOrderToMap)
			.collect(Collectors.toList());
		if(persistedSortOrder.size() > 0)
		{
			entryValues.put(COLUMN_SORT_ORDER, persistedSortOrder);
		}
	}
	
	private Map<String, String> sortOrderToMap(final GridSortOrder so)
	{
		final Map<String, String> map = new HashMap<>();
		map.put("key", so.getSorted().getKey());
		map.put("direction", so.getDirection().name());
		return map;
	}
	
	@Override
	public void restore(final Grid grid, final GuiPersistenceEntry entry)
	{
		super.restore(grid, entry);
		
		restoreColumnSortOrder(grid, entry);
		restoreColumnWidth(grid, entry);
	}

	@SuppressWarnings("unchecked")
	private void restoreColumnWidth(final Grid grid, final GuiPersistenceEntry entry)
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
	private void restoreColumnSortOrder(final Grid grid, final GuiPersistenceEntry entry)
	{
		final List<Map<String, String>> sortList = (List<Map<String, String>>)entry.value(COLUMN_SORT_ORDER);
		if(sortList != null)
		{
			final List<GridSortOrder> sortOrder = sortList.stream()
				.map(map -> mapToSortOrder(grid, map))
				.filter(Objects::nonNull)
				.collect(Collectors.toList());
			grid.sort(sortOrder);
		}
	}
	
	@SuppressWarnings("unchecked")
	private GridSortOrder mapToSortOrder(final Grid grid, final Map<String, String> map)
	{
		final Column column = grid.getColumnByKey(map.get("key"));
		return column != null
			? new GridSortOrder(column, SortDirection.valueOf(map.get("direction")))
			: null;
	}
}

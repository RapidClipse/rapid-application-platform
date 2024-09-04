/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.reports.grid;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.rapidclipse.framework.server.reports.grid.column.ColumnConfiguration;
import com.vaadin.flow.component.ItemLabelGenerator;
import com.vaadin.flow.component.grid.ColumnPathRenderer;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.data.provider.Query;
import com.vaadin.flow.data.provider.QuerySortOrder;
import com.vaadin.flow.data.renderer.BasicRenderer;
import com.vaadin.flow.data.renderer.Renderer;
import com.vaadin.flow.data.renderer.TextRenderer;
import com.vaadin.flow.function.SerializableComparator;
import com.vaadin.flow.function.ValueProvider;

import net.sf.dynamicreports.report.datasource.DRDataSource;
import net.sf.jasperreports.engine.JRDataSource;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public interface GridDataSourceFactory<T>
{
	public JRDataSource createDataSource(GridExportConfiguration<T> configuration);
	
	public static <T> GridDataSourceFactory<T> New()
	{
		return new Default<>();
	}

	public static class Default<T> implements GridDataSourceFactory<T>
	{
		@Override
		public JRDataSource createDataSource(final GridExportConfiguration<T> configuration)
		{
			final List<ColumnConfiguration<T>> columns = configuration.getColumnConfigurations();
			
			final DRDataSource dataSource = new DRDataSource(
				columns.stream()
					.filter(ColumnConfiguration::isVisible)
					.map(c -> c.getGridColumn().getKey())
					.toArray(String[]::new));

			this.getSortedAndFilteredData(configuration.getGrid()).forEach(item -> {
				
				final Object[] rowData = columns.stream()
					.filter(ColumnConfiguration::isVisible)
					.map(column -> this.getFormattedValue(column.getGridColumn(), item))
					.toArray();
				dataSource.add(rowData);
				
			});
			
			return dataSource;
		}
		
		@SuppressWarnings("unchecked")
		private String getFormattedValue(final Column<T> column, final T item)
		{
			try
			{
				final Renderer<T>         renderer          = column.getRenderer();
				final Method              getValueFormatter = this.getValueFormatter(renderer);
				final ValueProvider<T, ?> valueProvider     = this.getValueProvider(column);
				if(valueProvider != null)
				{
					final Object value = valueProvider.apply(item);
					if(value != null && getValueFormatter != null)
					{
						return (String)getValueFormatter.invoke(renderer, value);
					}
				}
				else if(renderer instanceof TextRenderer)
				{
					final Field itemLabelGenerator = TextRenderer.class.getDeclaredField("itemLabelGenerator");
					itemLabelGenerator.setAccessible(true);
					return ((ItemLabelGenerator<T>)itemLabelGenerator.get(renderer)).apply(item);
				}
				else if(renderer instanceof ColumnPathRenderer)
				{
					final Field provider = ColumnPathRenderer.class.getDeclaredField("provider");
					provider.setAccessible(true);
					final ValueProvider<T, ?> valprov = (ValueProvider<T, ?>)provider.get(renderer);
					if(valprov != null)
					{
						return valprov.apply(item).toString();
					}
				}
			}
			catch(IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchFieldException
				| SecurityException e)
			{
				e.printStackTrace();
			}
			return null;
		}
		
		private <RENDERER> Method getValueFormatter(final RENDERER renderer)
		{
			for(final Method m : renderer.getClass().getDeclaredMethods())
			{
				if(m.getName().contentEquals("getFormattedValue"))
				{
					m.setAccessible(true);
					return m;
				}
			}
			return null;
		}
		
		@SuppressWarnings("unchecked")
		private ValueProvider<T, ?> getValueProvider(final Column<T> column)
		{
			final Renderer<T> r = column.getRenderer();
			if(r instanceof BasicRenderer)
			{
				try
				{
					final Method getValueProvider = BasicRenderer.class.getDeclaredMethod("getValueProvider");
					getValueProvider.setAccessible(true);
					return (ValueProvider<T, ?>)getValueProvider.invoke(r);
				}
				catch(IllegalAccessException | IllegalArgumentException | InvocationTargetException
					| NoSuchMethodException | SecurityException e)
				{
					e.printStackTrace();
				}
			}
			return null;
		}
		
		@SuppressWarnings({"rawtypes", "unchecked"})
		private Stream<T> getSortedAndFilteredData(final Grid<T> grid)
		{
			final List<QuerySortOrder>      sortOrder       = grid.getSortOrder().stream()
				.flatMap(so -> so.getSorted().getSortOrder(so.getDirection()))
				.collect(Collectors.toList());
			final SerializableComparator<T> inMemorySorting = grid.getDataCommunicator()
				.getInMemorySorting();
			final Query                     query           = new Query<>(0, Integer.MAX_VALUE,
				sortOrder, inMemorySorting, null);
			return grid.getDataProvider().fetch(query);
		}
	}
}

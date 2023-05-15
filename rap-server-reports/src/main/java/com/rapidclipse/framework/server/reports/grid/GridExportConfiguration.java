/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.reports.grid;

import java.awt.Insets;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.data.renderer.RenderedComponent;
import com.rapidclipse.framework.server.reports.Format;
import com.rapidclipse.framework.server.reports.grid.column.ColumnConfiguration;
import com.rapidclipse.framework.server.reports.grid.column.ColumnConfigurationBuilder;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;

import net.sf.dynamicreports.report.constant.PageOrientation;
import net.sf.dynamicreports.report.constant.PageType;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public class GridExportConfiguration<T>
{
	/**
	 *
	 * @since 10.02.00
	 */
	public static <T> Predicate<Column<T>> DefaultColumnFilter()
	{
		return col -> !(col.getRenderer() instanceof RenderedComponent.RenderedComponentRenderer);
	}
	
	public static ColumnConfigurationBuilder getDefaultColumnConfigBuilder()
	{
		return new ColumnConfigurationBuilder()
			.withColumnConfigHeaderResolvingStrategyBuilder(headerResolvingStrategyBuilder -> {
				headerResolvingStrategyBuilder
					.withVaadinInternalHeaderStrategy()
					.withBeanKeyCaptionStrategy();
			});
	}
	
	private final Grid<T>                      grid;
	
	private final List<ColumnConfiguration<T>> columnConfigurations;
	private Format[]                           availableFormats = Format.All();
	private Format                             format           = Format.Pdf();
	private String                             title            = "";                        // No text = no title
	private PageType                           pageType         = PageType.A4;
	private PageOrientation                    pageOrientation  = PageOrientation.PORTRAIT;
	private Insets                             pageMargin       = new Insets(20, 20, 20, 20);
	private boolean                            showPageNumber   = false;
	private boolean                            highlightRows    = false;
	
	public GridExportConfiguration(final Grid<T> grid)
	{
		this(grid, null, null);
	}
	
	public GridExportConfiguration(final Grid<T> grid, final Predicate<Column<T>> columnFilter)
	{
		this(grid, columnFilter, null);
	}
	
	public GridExportConfiguration(
		final Grid<T> grid,
		Predicate<Column<T>> columnFilter,
		ColumnConfigurationBuilder configurationBuilder)
	{
		this.grid = grid;
		
		if(columnFilter == null)
		{
			columnFilter = DefaultColumnFilter();
		}
		if(configurationBuilder == null)
		{
			configurationBuilder = getDefaultColumnConfigBuilder();
		}
		
		// This has to be done here so that it's already available for the ExportDialog
		this.columnConfigurations = this.grid.getColumns().stream()
			.filter(columnFilter)
			.map(configurationBuilder::build)
			.collect(Collectors.toList());
	}
	
	/**
	 * @return the columnConfigurations
	 */
	public List<ColumnConfiguration<T>> getColumnConfigurations()
	{
		return this.columnConfigurations;
	}
	
	public Grid<T> getGrid()
	{
		return this.grid;
	}
	
	public GridExportConfiguration<T> setAvailableFormats(final Format... availableFormats)
	{
		this.availableFormats = availableFormats;
		return this;
	}
	
	public Format[] getAvailableFormats()
	{
		return this.availableFormats;
	}
	
	public GridExportConfiguration<T> setFormat(final Format format)
	{
		this.format = format;
		return this;
	}
	
	public Format getFormat()
	{
		return this.format;
	}
	
	public String getTitle()
	{
		return this.title;
	}
	
	public GridExportConfiguration<T> setTitle(final String title)
	{
		this.title = title;
		return this;
	}
	
	public PageType getPageType()
	{
		return this.pageType;
	}
	
	public GridExportConfiguration<T> setPageType(final PageType pageType)
	{
		this.pageType = pageType;
		return this;
	}
	
	public PageOrientation getPageOrientation()
	{
		return this.pageOrientation;
	}
	
	public GridExportConfiguration<T> setPageOrientation(final PageOrientation pageOrientation)
	{
		this.pageOrientation = pageOrientation;
		return this;
	}
	
	public Insets getPageMargin()
	{
		return this.pageMargin;
	}
	
	public GridExportConfiguration<T> setPageMargin(final Insets pageMargin)
	{
		this.pageMargin = pageMargin;
		return this;
	}
	
	public boolean isShowPageNumber()
	{
		return this.showPageNumber;
	}
	
	public GridExportConfiguration<T> setShowPageNumber(final boolean showPageNumber)
	{
		this.showPageNumber = showPageNumber;
		return this;
	}
	
	public boolean isHighlightRows()
	{
		return this.highlightRows;
	}
	
	public GridExportConfiguration<T> setHighlightRows(final boolean highlightRows)
	{
		this.highlightRows = highlightRows;
		return this;
	}
}

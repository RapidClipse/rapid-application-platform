/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.reports.grid;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.reports.grid.column.ColumnConfiguration;
import com.vaadin.flow.component.grid.ColumnTextAlign;

import net.sf.dynamicreports.jasper.builder.JasperReportBuilder;
import net.sf.dynamicreports.report.builder.DynamicReports;
import net.sf.dynamicreports.report.builder.column.Columns;
import net.sf.dynamicreports.report.builder.column.TextColumnBuilder;
import net.sf.dynamicreports.report.builder.component.Components;
import net.sf.dynamicreports.report.constant.HorizontalTextAlignment;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public interface GridReportBuilder<T>
{
	public JasperReportBuilder buildReport(GridExportConfiguration<T> configuration);
	
	public static <T> GridReportBuilder<T> New()
	{
		return new Default<>(GridDataSourceFactory.New(), GridReportStyles.New());
	}
	
	public static <T> GridReportBuilder<T> New(final GridDataSourceFactory<T> dataSourceFactory)
	{
		return new Default<>(dataSourceFactory, GridReportStyles.New());
	}
	
	public static <T> GridReportBuilder<T> New(final GridReportStyles styles)
	{
		return new Default<>(GridDataSourceFactory.New(), styles);
	}
	
	public static <T> GridReportBuilder<T> New(
		final GridDataSourceFactory<T> dataSourceFactory,
		final GridReportStyles styles)
	{
		return new Default<>(dataSourceFactory, styles);
	}
	
	public static class Default<T> implements GridReportBuilder<T>
	{
		protected final GridDataSourceFactory<T> dataSourceFactory;
		protected final GridReportStyles         styles;
		
		Default(
			final GridDataSourceFactory<T> dataSourceFactory,
			final GridReportStyles styles)
		{
			this.dataSourceFactory = dataSourceFactory;
			this.styles            = styles;
		}
		
		@Override
		public JasperReportBuilder buildReport(final GridExportConfiguration<T> configuration)
		{
			final JasperReportBuilder report = DynamicReports.report();
			
			configuration.getColumnConfigurations().stream()
				.filter(ColumnConfiguration::isVisible)
				.map(this::toReportColumn)
				.forEach(report::addColumn);
			
			report.setColumnTitleStyle(this.styles.columnTitleStyle());
			report.setColumnStyle(this.styles.columnStyle());
			
			final String title = configuration.getTitle();
			if(!StringUtils.isEmpty(title))
			{
				report.title(Components.text(title).setStyle(this.styles.titleStyle()));
				report.setReportName(title);
			}
			else
			{
				report.setReportName("GridExport");
			}
			
			if(configuration.isShowPageNumber())
			{
				report.pageFooter(DynamicReports.cmp.pageXofY().setStyle(this.styles.footerStyle()));
			}
			
			if(configuration.isHighlightRows())
			{
				report.highlightDetailOddRows();
			}
			
			report.setIgnorePagination(!configuration.getFormat().isPaginationActive());
			
			report.setShowColumnTitle(true);
			report.setDataSource(this.dataSourceFactory.createDataSource(configuration));
			report.setPageFormat(configuration.getPageType(), configuration.getPageOrientation());
			
			report.setPageMargin(
				configuration.getFormat().hasPageMargin() ? DynamicReports.margin(20) : DynamicReports.margin(0));
			
			return report;
		}
		
		private TextColumnBuilder<String> toReportColumn(final ColumnConfiguration<T> column)
		{
			final TextColumnBuilder<String> reportColumn = Columns
				.column(column.getHeader(), column.getGridColumn().getKey(), String.class);
			
			final Integer                   width        = column.getColumnWidth();
			if(width != null && width > 0)
			{
				reportColumn.setFixedWidth(width);
			}
			
			reportColumn.setHorizontalTextAlignment(
				this.toReportTextAlignment(column.getColumnAlignment()));
			
			return reportColumn;
		}
		
		private HorizontalTextAlignment toReportTextAlignment(final ColumnTextAlign alignment)
		{
			switch(alignment)
			{
				case END:
					return HorizontalTextAlignment.RIGHT;
				
				case CENTER:
					return HorizontalTextAlignment.CENTER;
				
				default:
					return HorizontalTextAlignment.LEFT;
			}
		}
	}
}

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

import static java.util.Objects.requireNonNull;

import java.awt.Insets;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.flowingcode.vaadin.addons.ironicons.IronIcons;
import com.rapidclipse.framework.server.data.renderer.RenderedComponent;
import com.rapidclipse.framework.server.reports.Format;
import com.rapidclipse.framework.server.reports.grid.column.ColumnConfiguration;
import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.ItemLabelGeneratorFactory;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import com.vaadin.flow.component.orderedlayout.FlexLayout.FlexWrap;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.page.Page;
import com.vaadin.flow.component.textfield.NumberField;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.router.AfterNavigationEvent;
import com.vaadin.flow.router.AfterNavigationObserver;
import com.vaadin.flow.server.StreamResource;

import net.sf.dynamicreports.jasper.builder.JasperReportBuilder;
import net.sf.dynamicreports.report.constant.PageOrientation;
import net.sf.dynamicreports.report.constant.PageType;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public class GridExportDialog<T> extends Dialog implements AfterNavigationObserver
{
	public static <T> GridExportDialog<T> open(final Grid<T> gridToExport)
	{
		return open(new GridExportConfiguration<>(gridToExport));
	}
	
	public static <T> GridExportDialog<T> open(final GridExportConfiguration<T> configuration)
	{
		final GridExportDialog<T> dialog = new GridExportDialog<>(configuration);
		dialog.open();
		return dialog;
	}
	
	private final GridExportConfiguration<T> configuration;
	private GridReportBuilder<T>             gridReportBuilder = GridReportBuilder.New();
	
	public GridExportDialog(final GridExportConfiguration<T> configuration)
	{
		super();
		
		this.configuration = configuration;
		
		this.initUI();
		
		this.txtReportTitle.setValue(configuration.getTitle());
		
		final Format[] formats = configuration.getAvailableFormats();
		Arrays.sort(formats, (f1, f2) -> f1.name().compareTo(f2.name()));
		
		this.cmbExportFormat.setItems(formats);
		if(formats.length > 0)
		{
			final Format presetFormat = configuration.getFormat();
			this.cmbExportFormat.setValue(presetFormat != null ? presetFormat : formats[0]);
		}
		this.cmbExportFormat.setItemLabelGenerator(Format::name);
		
		this.cmbPageOrientation.setItems(PageOrientation.values());
		this.cmbPageOrientation.setValue(configuration.getPageOrientation());
		this.cmbPageOrientation.setItemLabelGenerator(orientation -> StringResourceUtils
			.optLocalizeString("{$PageOrientation." + orientation.name() + "}", this));
		
		this.cmbPageFormat.setItems(PageType.values());
		this.cmbPageFormat.setValue(configuration.getPageType());
		
		this.grid.getColumnByKey("visible").setHeader(new Icon(VaadinIcon.EYE));
		this.grid.setItems(configuration.getColumnConfigurations());
		this.grid.recalculateColumnWidths();
		
		this.ckShowPageNumbers.setValue(configuration.isShowPageNumber());
		this.ckHighlightRows.setValue(configuration.isHighlightRows());
		
		final Page page = UI.getCurrent().getPage();
		page.retrieveExtendedClientDetails(e -> this.adjustGridColumns(e.getBodyClientWidth()));
		page.addBrowserWindowResizeListener(e -> this.adjustGridColumns(e.getWidth()));
	}
	
	public void setGridReportBuilder(final GridReportBuilder<T> gridReportBuilder)
	{
		this.gridReportBuilder = requireNonNull(gridReportBuilder);
	}
	
	public GridReportBuilder<T> getGridReportBuilder()
	{
		return this.gridReportBuilder;
	}
	
	private void adjustGridColumns(final int width)
	{
		final boolean visible = width >= 666;
		this.grid.getColumnByKey("width").setVisible(visible);
		this.grid.getColumnByKey("position").setVisible(visible);
	}
	
	private void moveUp(final ColumnConfiguration<T> column)
	{
		final List<ColumnConfiguration<T>> columns = this.configuration.getColumnConfigurations();
		final int                          index   = columns.indexOf(column);
		if(index > 0 && columns.size() > 1)
		{
			Collections.swap(columns, index - 1, index);
		}
		this.grid.getDataProvider().refreshAll();
	}
	
	private void moveDown(final ColumnConfiguration<T> column)
	{
		final List<ColumnConfiguration<T>> columns = this.configuration.getColumnConfigurations();
		final int                          index   = columns.indexOf(column);
		if(index < columns.size() - 1 && columns.size() > 1)
		{
			Collections.swap(columns, index, index + 1);
		}
		this.grid.getDataProvider().refreshAll();
	}
	
	private boolean check()
	{
		boolean ok    = false;
		String  error = "";
		
		if(checkColumnSelection())
		{
			if(checkColumnWidth())
			{
				if(this.cmbExportFormat.getValue() != null)
				{
					ok = true;
				}
			}
			else
			{
				error = StringResourceUtils.optLocalizeString("{$widthTooBigError}", this);
			}
		}
		
		this.btnExport.setEnabled(ok);
		this.lblStatus.setText(error);
		
		return ok;
	}
	
	private boolean checkColumnWidth()
	{
		if(this.cmbPageOrientation.getValue() != null && this.cmbPageFormat.getValue() != null)
		{
			final Insets margin = this.configuration.getPageMargin();
			if(this.cmbPageOrientation.getValue().equals(PageOrientation.PORTRAIT))
			{
				if(this.calculateFixedWidth() > this.cmbPageFormat.getValue().getWidth() - margin.left - margin.right)
				{
					return false;
				}
			}
			else
			{
				if(this.calculateFixedWidth() > this.cmbPageFormat.getValue().getHeight() - margin.left - margin.right)
				{
					return false;
				}
			}
		}
		
		return true;
	}
	
	private int calculateFixedWidth()
	{
		return this.configuration.getColumnConfigurations().stream()
			.filter(ColumnConfiguration::isVisible)
			.map(ColumnConfiguration::getColumnWidth)
			.filter(Objects::nonNull)
			.mapToInt(Integer::intValue)
			.sum();
	}
	
	private boolean checkColumnSelection()
	{
		return this.configuration.getColumnConfigurations().stream()
			.anyMatch(ColumnConfiguration::isVisible);
	}
	
	private StreamResource createReportResource()
	{
		final Format format = this.cmbExportFormat.getValue();
		this.configuration.setFormat(format);
		this.configuration.setHighlightRows(this.ckHighlightRows.getValue());
		this.configuration.setPageOrientation(this.cmbPageOrientation.getValue());
		this.configuration.setPageType(this.cmbPageFormat.getValue());
		this.configuration.setShowPageNumber(this.ckShowPageNumbers.getValue());
		this.configuration.setTitle(this.txtReportTitle.getValue());
		
		final JasperReportBuilder builder = this.gridReportBuilder.buildReport(this.configuration);
		return format.createExporter().exportToResource(builder, this.configuration.getTitle());
	}
	
	private void setAllColumnsVisible(final boolean visible)
	{
		this.configuration.getColumnConfigurations().forEach(conf -> conf.setVisible(visible));
		this.grid.getDataProvider().refreshAll();
		this.check();
	}
	
	private void export()
	{
		if(!this.check())
		{
			return;
		}
		
		final StreamResource res = this.createReportResource();
		new ReportViewerDialog(res, this.cmbExportFormat.getValue()).open();
	}
	
	@Override
	public void afterNavigation(final AfterNavigationEvent event)
	{
		// Workaround for https://github.com/vaadin/vaadin-dialog-flow/issues/108
		this.close();
	}
	
	private void initUI()
	{
		this.layout                  = new VerticalLayout();
		this.titlebar                = new HorizontalLayout();
		this.iconGrid                = new Icon(VaadinIcon.TABLE);
		this.lblTitle                = new Label();
		this.txtReportTitle          = new TextField();
		this.lblGridTitle            = new Label();
		this.gridcontent             = new VerticalLayout();
		this.gridcontainer           = new FlexLayout();
		this.grid                    = new Grid<ColumnConfiguration<T>>();
		this.gridselectors           = new VerticalLayout();
		this.btnSelectAll            = new Button();
		this.btnClearSelection       = new Button();
		this.optionscontainer        = new FlexLayout();
		this.cmbPageFormat           = new ComboBox<>();
		this.cmbPageOrientation      = new ComboBox<>();
		this.configurationcheckboxes = new VerticalLayout();
		this.ckShowPageNumbers       = new Checkbox();
		this.ckHighlightRows         = new Checkbox();
		this.cmbExportFormat         = new ComboBox<>();
		this.bottomlayout            = new HorizontalLayout();
		this.lblStatus               = new Label();
		this.buttonbar               = new HorizontalLayout();
		this.btnCancel               = new Button();
		this.btnExport               = new Button();
		
		this.layout.setSpacing(false);
		this.layout.setMaxHeight("100%");
		this.layout.setMaxWidth("100%");
		this.layout.setPadding(false);
		this.lblTitle.setText(StringResourceUtils.optLocalizeString("{$caption}", this));
		this.txtReportTitle.setMaxWidth("100%");
		this.txtReportTitle.setLabel(StringResourceUtils.optLocalizeString("{$title}", this));
		this.lblGridTitle.setText(StringResourceUtils.optLocalizeString("{$columns}", this));
		this.lblGridTitle.getStyle().set("padding-top", "10px");
		this.gridcontainer.setFlexWrap(FlexWrap.WRAP);
		this.grid.setMaxWidth("100%");
		this.grid.addThemeVariants(GridVariant.LUMO_COLUMN_BORDERS, GridVariant.LUMO_ROW_STRIPES);
		this.grid
			.addColumn(RenderedComponent.Renderer(VisibilityRenderer::new))
			.setKey("visible").setHeader(" ").setResizable(true).setSortable(false).setWidth("60px").setFlexGrow(0);
		this.grid.addColumn(RenderedComponent.Renderer(HeaderRenderer::new))
			.setKey("name")
			.setHeader(StringResourceUtils.optLocalizeString("{$columnName}", this)).setResizable(true)
			.setSortable(false)
			.setWidth("350px").setAutoWidth(true);
		this.grid.addColumn(RenderedComponent.Renderer(WidthRenderer::new))
			.setKey("width").setHeader(StringResourceUtils.optLocalizeString("{$columnWidth}", this)).setResizable(true)
			.setSortable(false).setWidth("100px").setFlexGrow(0);
		this.grid
			.addColumn(RenderedComponent.Renderer(PositionRenderer::new))
			.setKey("position").setHeader("Position").setSortable(false).setWidth("150px").setFlexGrow(0);
		this.grid.setSelectionMode(Grid.SelectionMode.SINGLE);
		this.gridselectors.setDefaultHorizontalComponentAlignment(FlexComponent.Alignment.STRETCH);
		this.btnSelectAll.setText(StringResourceUtils.optLocalizeString("{$selectAll}", this));
		this.btnClearSelection.setText(StringResourceUtils.optLocalizeString("{$selectNone}", this));
		this.optionscontainer.setFlexWrap(FlexWrap.WRAP);
		this.cmbPageFormat.setLabel(StringResourceUtils.optLocalizeString("{$pageFormat}", this));
		this.cmbPageFormat.getStyle().set("padding-right", "10px");
		this.cmbPageFormat.setItemLabelGenerator(ItemLabelGeneratorFactory.NonNull(CaptionUtils::resolveCaption));
		this.cmbPageOrientation.setLabel(StringResourceUtils.optLocalizeString("{$orientation}", this));
		this.cmbPageOrientation.getStyle().set("padding-right", "10px");
		this.cmbPageOrientation.setItemLabelGenerator(ItemLabelGeneratorFactory.NonNull(CaptionUtils::resolveCaption));
		this.configurationcheckboxes.getStyle().set("padding-right", "10px");
		this.ckShowPageNumbers.setLabel(StringResourceUtils.optLocalizeString("{$showPageNumbers}", this));
		this.ckHighlightRows.setLabel(StringResourceUtils.optLocalizeString("{$highlightRows}", this));
		this.cmbExportFormat.setLabel(StringResourceUtils.optLocalizeString("{$format}", this));
		this.cmbExportFormat.setItemLabelGenerator(ItemLabelGeneratorFactory.NonNull(CaptionUtils::resolveCaption));
		this.bottomlayout.setSpacing(false);
		this.bottomlayout.setJustifyContentMode(FlexComponent.JustifyContentMode.BETWEEN);
		this.lblStatus.getStyle().set("color", "red");
		this.buttonbar.setJustifyContentMode(FlexComponent.JustifyContentMode.END);
		this.btnCancel.setText(StringResourceUtils.optLocalizeString("{$cancel}", this));
		this.btnExport.setText(StringResourceUtils.optLocalizeString("{$btnExport}", this));
		this.btnExport.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
		this.btnClose = new Button(VaadinIcon.CLOSE.create());
		
		this.titlebar.add(this.iconGrid, this.lblTitle, this.btnClose);
		this.titlebar.setWidthFull();
		this.titlebar.setHeight(null);
		this.titlebar.setDefaultVerticalComponentAlignment(FlexComponent.Alignment.CENTER);
		this.titlebar.setFlexGrow(1.0, this.lblTitle);
		
		this.btnSelectAll.setWidthFull();
		this.btnSelectAll.setHeight(null);
		this.btnClearSelection.setWidthFull();
		this.btnClearSelection.setHeight(null);
		this.gridselectors.add(this.btnSelectAll, this.btnClearSelection);
		this.grid.setWidth("600px");
		this.grid.setHeight("400px");
		this.gridselectors.setSizeUndefined();
		this.gridcontainer.add(this.grid, this.gridselectors);
		this.gridcontainer.setFlexGrow(1.0, this.grid);
		this.ckShowPageNumbers.setSizeUndefined();
		this.ckHighlightRows.setSizeUndefined();
		this.configurationcheckboxes.add(this.ckShowPageNumbers, this.ckHighlightRows);
		this.cmbPageFormat.setSizeUndefined();
		this.cmbPageOrientation.setSizeUndefined();
		this.configurationcheckboxes.setSizeUndefined();
		this.cmbExportFormat.setSizeUndefined();
		this.optionscontainer.add(this.configurationcheckboxes, this.cmbPageFormat, this.cmbPageOrientation,
			this.cmbExportFormat);
		this.gridcontainer.setWidthFull();
		this.gridcontainer.setHeight(null);
		this.optionscontainer.setSizeUndefined();
		this.gridcontent.add(this.gridcontainer, this.optionscontainer);
		this.btnCancel.setSizeUndefined();
		this.btnExport.setSizeUndefined();
		this.buttonbar.add(this.btnCancel, this.btnExport);
		this.lblStatus.setWidthFull();
		this.lblStatus.setHeight(null);
		this.buttonbar.setWidthFull();
		this.buttonbar.setHeight(null);
		this.bottomlayout.add(this.lblStatus, this.buttonbar);
		this.txtReportTitle.setWidth("400px");
		this.txtReportTitle.setHeight(null);
		this.lblGridTitle.setSizeUndefined();
		this.gridcontent.setWidthFull();
		this.gridcontent.setHeight(null);
		this.bottomlayout.setWidthFull();
		this.bottomlayout.setHeight(null);
		this.layout.add(this.titlebar, this.txtReportTitle, this.lblGridTitle, this.gridcontent, this.bottomlayout);
		this.layout.setWidth("1000px");
		this.layout.setHeight(null);
		this.add(this.layout);
		this.setSizeUndefined();
		
		this.txtReportTitle.addValueChangeListener(event -> check());
		this.btnSelectAll.addClickListener(event -> setAllColumnsVisible(true));
		this.btnClearSelection.addClickListener(event -> setAllColumnsVisible(false));
		this.cmbPageFormat.addValueChangeListener(event -> check());
		this.cmbPageOrientation.addValueChangeListener(event -> check());
		this.cmbExportFormat.addValueChangeListener(event -> check());
		this.btnCancel.addClickListener(event -> close());
		this.btnClose.addClickListener(event -> close());
		this.btnExport.addClickListener(event -> export());
	}
	
	private Checkbox                     ckShowPageNumbers, ckHighlightRows;
	private Button                       btnSelectAll, btnClearSelection, btnCancel, btnExport;
	private FlexLayout                   optionscontainer, gridcontainer;
	private Grid<ColumnConfiguration<T>> grid;
	private VerticalLayout               layout, gridcontent, configurationcheckboxes, gridselectors;
	private HorizontalLayout             titlebar, bottomlayout, buttonbar;
	private Label                        lblTitle, lblGridTitle, lblStatus;
	private Button                       btnClose;
	private ComboBox<PageType>           cmbPageFormat;
	private ComboBox<Format>             cmbExportFormat;
	private Icon                         iconGrid;
	private TextField                    txtReportTitle;
	private ComboBox<PageOrientation>    cmbPageOrientation;
	
	private class VisibilityRenderer extends Checkbox implements RenderedComponent<ColumnConfiguration<T>>
	{
		@Override
		public void renderComponent(final ColumnConfiguration<T> value)
		{
			this.setValue(value.isVisible());
			this.addValueChangeListener(e -> {
				value.setVisible(e.getValue());
				GridExportDialog.this.check();
			});
		}
	}
	
	private class HeaderRenderer extends TextField implements RenderedComponent<ColumnConfiguration<T>>
	{
		HeaderRenderer()
		{
			super();
			
			this.setRequired(true);
			this.setWidthFull();
		}
		
		@Override
		public void renderComponent(final ColumnConfiguration<T> value)
		{
			this.setValue(value.getHeader());
			this.addValueChangeListener(e -> value.setHeader(e.getValue()));
		}
	}
	
	private class WidthRenderer extends NumberField implements RenderedComponent<ColumnConfiguration<T>>
	{
		WidthRenderer()
		{
			super();
			
			this.setWidthFull();
		}
		
		@Override
		public void renderComponent(final ColumnConfiguration<T> value)
		{
			final Integer columnWidth = value.getColumnWidth();
			this.setValue(columnWidth != null ? columnWidth.doubleValue() : null);
			this.addValueChangeListener(e -> {
				final Double d = e.getValue();
				value.setColumnWidth(d != null ? d.intValue() : null);
				GridExportDialog.this.check();
			});
		}
	}
	
	private class PositionRenderer extends HorizontalLayout
		implements RenderedComponent<ColumnConfiguration<T>>
	{
		private final Button up, down;
		
		PositionRenderer()
		{
			super();
			
			this.up   = new Button(IronIcons.ARROW_UPWARD.create());
			this.down = new Button(IronIcons.ARROW_DOWNWARD.create());
			
			this.add(this.up, this.down);
			this.setSizeFull();
			this.setMargin(false);
			this.setPadding(false);
		}
		
		@Override
		public void renderComponent(final ColumnConfiguration<T> value)
		{
			this.down.addClickListener(e -> GridExportDialog.this.moveDown(value));
			this.up.addClickListener(e -> GridExportDialog.this.moveUp(value));
		}
	}
}

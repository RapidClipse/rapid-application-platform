/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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

import java.beans.Beans;

import com.rapidclipse.framework.server.reports.Format;
import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.DownloadAnchor;
import com.vaadin.flow.component.accordion.Accordion;
import com.vaadin.flow.component.accordion.AccordionPanel;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.HtmlObject;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.AfterNavigationEvent;
import com.vaadin.flow.router.AfterNavigationObserver;
import com.vaadin.flow.server.StreamResource;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public class ReportViewerDialog extends Dialog implements AfterNavigationObserver
{
	private final VerticalLayout   rootLayout            = new VerticalLayout();
	
	private final HorizontalLayout headerbar             = new HorizontalLayout();
	private final Button           btnClose              = new Button(VaadinIcon.CLOSE.create());
	private final DownloadAnchor   downloadAnchor        = new DownloadAnchor();
	private final Label            lblTitle              = new Label("Report");
	
	private final Accordion        previewAccordion      = new Accordion();
	private final AccordionPanel   previewAccordionPanel =
		new AccordionPanel(StringResourceUtils.optLocalizeString("{$preview}", this), new Div());
	
	private final HtmlObject       resViewer;
	
	/**
	 *
	 * @param res
	 * @param mimeType
	 * @deprecated Use {@link #ReportViewerDialog(StreamResource, ExportFormat)}
	 */
	@Deprecated
	public ReportViewerDialog(final StreamResource res, final String mimeType)
	{
		this(res, mimeType, true);
	}
	
	/**
	 *
	 * @param res
	 * @param mimeType
	 * @param isPreviewableInStandardBrowser
	 * @deprecated Will be moved to {@link #ReportViewerDialog(StreamResource, ExportFormat)}
	 */
	@Deprecated
	protected ReportViewerDialog(
		final StreamResource res,
		final String mimeType,
		final boolean isPreviewableInStandardBrowser)
	{
		this.initUI();
		
		if(!Beans.isDesignTime())
		{
			this.lblTitle.setText(res.getName());
			
			this.resViewer = new HtmlObject(res, mimeType);
			this.resViewer.setMinHeight("60vh");
			this.resViewer.setSizeFull();
			this.resViewer.setMaxWidth("100%");
			this.resViewer.setMaxHeight("100%");
			this.resViewer.getElement().setText(StringResourceUtils.optLocalizeString("{$unableToShowPreview}", this));
			
			this.previewAccordionPanel.setContent(this.resViewer);
			if(isPreviewableInStandardBrowser)
			{
				this.previewAccordion.open(this.previewAccordionPanel);
			}
			else
			{
				this.previewAccordion.close();
			}
			
			final Button btnDownload =
				new Button(StringResourceUtils.optLocalizeString("{$download}", this), VaadinIcon.DOWNLOAD.create());
			btnDownload.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
			this.downloadAnchor.add(btnDownload);
			this.downloadAnchor.setResource(res);
		}
		else
		{
			// Must be assigned, other wise the field can't be final
			this.resViewer = null;
		}
	}
	
	public ReportViewerDialog(final StreamResource res, final Format format)
	{
		this(res, format.mimeType(), format.isPreviewableInStandardBrowser());
	}
	
	private void initUI()
	{
		this.btnClose.addClickListener(event -> close());
		
		this.headerbar.setSpacing(true);
		this.headerbar.setPadding(false);
		this.headerbar.setDefaultVerticalComponentAlignment(FlexComponent.Alignment.CENTER);
		this.headerbar.setFlexGrow(1.0, this.lblTitle);
		this.headerbar.setWidthFull();
		this.headerbar.add(this.lblTitle, this.downloadAnchor, this.btnClose);
		
		this.previewAccordion.setSizeFull();
		this.previewAccordion.add(this.previewAccordionPanel);
		
		this.rootLayout.setSpacing(true);
		this.rootLayout.setPadding(false);
		this.rootLayout.setWidth("70vw");
		this.rootLayout.setMaxHeight("80vh");
		this.rootLayout.add(this.headerbar, this.previewAccordion);
		
		this.add(this.rootLayout);
		this.setSizeUndefined();
	}
	
	/**
	 * @return the rootLayout
	 */
	public VerticalLayout getRootLayout()
	{
		return this.rootLayout;
	}
	
	/**
	 * @return the resViewer
	 */
	public HtmlObject getResViewer()
	{
		return this.resViewer;
	}
	
	@Override
	public void afterNavigation(final AfterNavigationEvent event)
	{
		// Workaround for https://github.com/vaadin/vaadin-dialog-flow/issues/108
		this.close();
	}
}

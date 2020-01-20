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
package com.rapidclipse.framework.server.reports.grid;

import java.beans.Beans;

import com.rapidclipse.framework.server.ui.DownloadAnchor;
import com.rapidclipse.framework.server.ui.HtmlObject;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.server.StreamResource;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public class ReportViewerDialog extends Dialog
{
	public ReportViewerDialog(final StreamResource res, final String mimeType)
	{
		super();
		
		this.initUI();
		
		if(!Beans.isDesignTime())
		{
			this.lblTitle.setText(res.getName());
			final HtmlObject resViewer = new HtmlObject(res, mimeType);
			resViewer.setWidth("1000px");
			resViewer.setHeight("700px");
			resViewer.setMaxWidth("100%");
			resViewer.setMaxHeight("100%");
			this.rootLayout.add(resViewer);
			
			this.downloadAnchor.setResource(res);
			this.downloadAnchor.add(new Button("Download", VaadinIcon.DOWNLOAD.create()));
		}
	}
	
	private void initUI()
	{
		this.rootLayout     = new VerticalLayout();
		this.headerbar      = new HorizontalLayout();
		this.lblTitle       = new Label();
		this.btnClose       = new Button(VaadinIcon.CLOSE.create());
		this.downloadAnchor = new DownloadAnchor();
		
		this.rootLayout.setSpacing(true);
		this.rootLayout.setPadding(false);
		this.headerbar.setSpacing(true);
		this.headerbar.setPadding(false);
		
		this.lblTitle.setText("Report");
		
		this.headerbar.add(this.lblTitle, this.downloadAnchor, this.btnClose);
		this.headerbar.setWidthFull();
		this.rootLayout.add(this.headerbar);
		this.rootLayout.setSizeFull();
		this.add(this.rootLayout);
		this.setSizeUndefined();
		
		this.headerbar.setDefaultVerticalComponentAlignment(FlexComponent.Alignment.CENTER);
		this.headerbar.setFlexGrow(1.0, this.lblTitle);
		
		this.btnClose.addClickListener(event -> close());
	}
	
	private Button           btnClose;
	private DownloadAnchor   downloadAnchor;
	private VerticalLayout   rootLayout;
	private HorizontalLayout headerbar;
	private Label            lblTitle;
}

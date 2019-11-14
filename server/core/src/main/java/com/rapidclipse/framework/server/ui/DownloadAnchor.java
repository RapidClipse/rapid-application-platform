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

package com.rapidclipse.framework.server.ui;

import java.io.IOException;
import java.util.UUID;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.server.RequestHandler;
import com.vaadin.flow.server.StreamResource;
import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinResponse;
import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 * @since 10.01.00
 */
public class DownloadAnchor extends Anchor
{
	private final String   identifier = "download-" + UUID.randomUUID().toString();
	private RequestHandler requestHandler;
	private StreamResource resource;
	
	public DownloadAnchor()
	{
		super();
		
		runBeforeClientResponse(ui -> {
			this.requestHandler = new RequestHandler()
			{
				@Override
				public boolean handleRequest(
					final VaadinSession session,
					final VaadinRequest request,
					final VaadinResponse response)
					throws IOException
				{
					final String         identifier = DownloadAnchor.this.identifier;
					final StreamResource resource   = DownloadAnchor.this.resource;
					
					if(request.getPathInfo().endsWith(identifier) && resource != null)
					{
						response.setStatus(200);
						response.setHeader("Content-Disposition",
							"attachment; filename=\"" + resource.getName() + "\"");
						resource.getWriter().accept(response.getOutputStream(), session);
						return true;
					}
					return false;
				}
			};
			
			ui.getSession().addRequestHandler(this.requestHandler);
			
			setHref("./" + this.identifier);
		});
		
		addDetachListener(event -> {
			getUI().get().getSession().removeRequestHandler(this.requestHandler);
		});
	}
	
	private void runBeforeClientResponse(final SerializableConsumer<UI> command)
	{
		getElement().getNode().runWhenAttached(
			ui -> ui.beforeClientResponse(this, context -> command.accept(ui)));
	}
	
	public DownloadAnchor(final StreamResource resource)
	{
		this();
		
		this.resource = resource;
	}
	
	public void setResource(final StreamResource resource)
	{
		this.resource = resource;
	}
	
	public StreamResource getResource()
	{
		return this.resource;
	}
}

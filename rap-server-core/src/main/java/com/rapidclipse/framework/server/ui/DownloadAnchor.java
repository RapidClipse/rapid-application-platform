/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui;

import java.io.IOException;
import java.util.UUID;

import com.vaadin.flow.component.PropertyDescriptor;
import com.vaadin.flow.component.PropertyDescriptors;
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
	private static final PropertyDescriptor<String, String> downloadDescriptor =
		PropertyDescriptors.attributeWithDefault("download", "", false);
	
	private final String   identifier = "download-" + UUID.randomUUID().toString();
	private RequestHandler requestHandler;
	private StreamResource resource;
	
	public DownloadAnchor()
	{
		super();
		this.set(DownloadAnchor.downloadDescriptor, "");
		
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

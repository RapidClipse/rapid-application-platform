/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.net;

import org.jsoup.nodes.Element;

import com.rapidclipse.framework.server.Rap;
import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.VaadinServiceInitListener;


/**
 * @author XDEV Software
 *
 */
public class ContentSecurityPolicyExtension implements VaadinServiceInitListener
{
	private final static String ATTRIBUTE_KEY   = "http-equiv";
	private final static String ATTRIBUTE_VALUE = "Content-Security-Policy";
	
	@Override
	public void serviceInit(final ServiceInitEvent event)
	{
		event.addBootstrapListener(response -> {
			
			final ContentSecurityPolicy contentSecurityPolicy = Rap.getContentSecurityPolicy();
			if(contentSecurityPolicy != null && !contentSecurityPolicy.isEmpty())
			{
				final Element head = response.getDocument().head();
				head.getElementsByAttributeValue(ATTRIBUTE_KEY, ATTRIBUTE_VALUE)
					.forEach(Element::remove);
				head.prependElement("meta").attr(ATTRIBUTE_KEY, ATTRIBUTE_VALUE).attr("content",
					ContentSecurityPolicy.Assembler().assemble(contentSecurityPolicy));
			}
		});
	}
}

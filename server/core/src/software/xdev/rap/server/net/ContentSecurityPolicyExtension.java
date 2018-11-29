/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.net;


import org.jsoup.nodes.Element;

import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.VaadinServiceInitListener;

import software.xdev.rap.server.Rap;


/**
 * @author XDEV Software
 *
 */
public class ContentSecurityPolicyExtension implements VaadinServiceInitListener
{
	private final static String	ATTRIBUTE_KEY	= "http-equiv";
	private final static String	ATTRIBUTE_VALUE	= "Content-Security-Policy";


	@Override
	public void serviceInit(final ServiceInitEvent event)
	{
		event.addBootstrapListener(response -> {

			final ContentSecurityPolicy contentSecurityPolicy = Rap.getContentSecurityPolicy();
			if(contentSecurityPolicy != null && !contentSecurityPolicy.isEmpty())
			{
				final Element head = response.getDocument().head();
				head.getElementsByAttributeValue(ATTRIBUTE_KEY,ATTRIBUTE_VALUE)
						.forEach(Element::remove);
				head.prependElement("meta").attr(ATTRIBUTE_KEY,ATTRIBUTE_VALUE).attr("content",
						ContentSecurityPolicy.Assembler().assemble(contentSecurityPolicy));
			}
		});
	}
}

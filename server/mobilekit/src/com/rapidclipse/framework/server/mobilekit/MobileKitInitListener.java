/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

package com.rapidclipse.framework.server.mobilekit;


import com.vaadin.flow.component.UI;
import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.VaadinServiceInitListener;


/**
 * @author XDEV Software
 *
 */
public class MobileKitInitListener implements VaadinServiceInitListener
{
	public MobileKitInitListener()
	{
		super();
	}


	@Override
	public void serviceInit(final ServiceInitEvent serviceEvent)
	{
		serviceEvent.addBootstrapListener(event -> {

			final MobileConfiguration configuration = event.getSession()
					.getAttribute(MobileConfiguration.class);
			if(configuration != null)
			{
				final UI ui = event.getUI();
				for(final MobileServiceConfiguration serviceConfiguration : configuration
						.getMobileServices())
				{
					try
					{
						final ServiceComponent serviceComponent = serviceConfiguration
								.getServiceClass().getAnnotation(ServiceComponent.class);
						if(serviceComponent != null)
						{
							final MobileComponent component = serviceComponent.value()
									.newInstance();
							ui.add(component);
						}
					}
					catch(final Exception e)
					{
						throw new RuntimeException(e);
					}
				}
			}
		});
	}
}

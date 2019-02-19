
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


package com.rapidclipse.framework.server.security;

import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.VaadinServiceInitListener;


/**
 * @author XDEV Software
 *
 */
public class AuthServiceInitListener implements VaadinServiceInitListener
{
	public AuthServiceInitListener()
	{
		super();
	}
	
	@Override
	public void serviceInit(final ServiceInitEvent event)
	{
		event.getSource().addUIInitListener(
			e -> e.getUI().addBeforeEnterListener(new AuthNavigationController()));
	}
}

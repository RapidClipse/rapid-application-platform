
package com.rapidclipse.framework.server.security.authorization;

import com.vaadin.flow.router.BeforeEvent;


/**
 * Handler which gets called when an unauthorized navigation request happens.
 *
 * @author XDEV Software
 */
@FunctionalInterface
public interface UnauthorizedNavigationRequestHandler
{
	/**
	 * Handles the unauthorized request, e.g. shows an error message and
	 * redirects.
	 *
	 * @param navigator
	 * @param event
	 */
	public void handle(BeforeEvent event);
	
	public static UnauthorizedNavigationRequestHandler Default()
	{
		return new Default();
	}
	
	public static class Default implements UnauthorizedNavigationRequestHandler
	{
		@Override
		public void handle(final BeforeEvent event)
		{
			Authorization.rerouteToPermissionDeniedView(event);
		}
	}
}

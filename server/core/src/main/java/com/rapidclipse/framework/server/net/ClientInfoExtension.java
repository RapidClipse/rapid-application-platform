
package com.rapidclipse.framework.server.net;

import com.rapidclipse.framework.server.RapServletService;
import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 *
 */
public class ClientInfoExtension implements RapServletService.Extension
{
	@Override
	public void sessionCreated(
		final RapServletService service,
		final VaadinSession session,
		final VaadinRequest request)
	{
		session.setAttribute(ClientInfo.class, ClientInfo.New(request));
	}
}

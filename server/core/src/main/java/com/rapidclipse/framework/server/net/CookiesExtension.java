
package com.rapidclipse.framework.server.net;

import javax.servlet.ServletException;

import com.rapidclipse.framework.server.RapServlet;


/**
 * @author XDEV Software
 *
 */
public class CookiesExtension implements RapServlet.Extension
{
	/**
	 *
	 */
	public CookiesExtension()
	{
		super();
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void servletInitialized(final RapServlet servlet) throws ServletException
	{
		servlet.getService().addSessionInitListener(event -> Cookies.initFor(event.getSession()));
	}
}

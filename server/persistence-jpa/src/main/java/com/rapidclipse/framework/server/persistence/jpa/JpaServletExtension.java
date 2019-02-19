
package com.rapidclipse.framework.server.persistence.jpa;

import javax.servlet.ServletException;

import com.rapidclipse.framework.server.RapServlet;


/**
 * @author XDEV Software
 *
 */
public class JpaServletExtension implements RapServlet.Extension
{
	public JpaServletExtension()
	{
		super();
	}
	
	@Override
	public void servletInitialized(final RapServlet servlet) throws ServletException
	{
		// Init JPA
		
		final PersistenceManager persistenceManager = Jpa.getPersistenceManager();
		for(final String persistenceUnit : persistenceManager.getPersistenceUnits())
		{
			persistenceManager.getEntityManagerFactory(persistenceUnit).createEntityManager()
				.close();
		}
	}
}

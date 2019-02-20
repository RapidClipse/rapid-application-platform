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

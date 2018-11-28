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

package software.xdev.rap.server.persistence.jpa;


import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaQuery;
import javax.servlet.ServletContext;

import org.apache.commons.lang3.StringUtils;

import software.xdev.rap.server.RapServlet;


/**
 * @author XDEV Software
 *
 */
public final class Jpa
{
	private static PersistenceManager		persistenceManager;
	private static SessionStrategyProvider	sessionStrategyProvider;
	
	
	/**
	 * @return the persistenceManager
	 */
	public static PersistenceManager getPersistenceManager()
	{
		if(persistenceManager == null)
		{
			persistenceManager = createPersistenceManager(
					RapServlet.getRapServlet().getServletContext());
		}
		
		return persistenceManager;
	}
	
	
	private static PersistenceManager createPersistenceManager(final ServletContext context)
	{
		final String className = context
				.getInitParameter(PersistenceManager.FACTORY_INIT_PARAMETER);
		if(!StringUtils.isEmpty(className))
		{
			try
			{
				final PersistenceManager.Factory factory = (PersistenceManager.Factory)Class
						.forName(className).newInstance();
				return factory.createPersistenceManager(context);
			}
			catch(final Throwable t)
			{
				throw new RuntimeException(t);
			}
		}
		
		return new PersistenceManager.Implementation(context);
	}
	
	
	/**
	 * @return the sessionStrategyProvider
	 */
	public static SessionStrategyProvider getSessionStrategyProvider()
	{
		if(sessionStrategyProvider == null)
		{
			sessionStrategyProvider = createSessionStrategyProvider(
					RapServlet.getRapServlet().getServletContext());
		}
		
		return sessionStrategyProvider;
	}
	
	
	private static SessionStrategyProvider createSessionStrategyProvider(
			final ServletContext context)
	{
		final String className = context
				.getInitParameter(SessionStrategyProvider.FACTORY_INIT_PARAMETER);
		if(!StringUtils.isEmpty(className))
		{
			try
			{
				final SessionStrategyProvider.Factory factory = (SessionStrategyProvider.Factory)Class
						.forName(className).newInstance();
				return factory.createSessionStrategyProvider(context);
			}
			catch(final Throwable t)
			{
				throw new RuntimeException(t);
			}
		}

		return new SessionStrategyProvider.Implementation();
	}
	
	
	/**
	 *
	 * @param managedType
	 * @return
	 */
	public static String getPersistenceUnit(final Class<?> managedType)
	{
		final PersistenceManager persistenceManager;
		if((persistenceManager = getPersistenceManager()) != null)
		{
			return persistenceManager.getPersistenceUnit(managedType);
		}
		
		return null;
	}
	
	
	/**
	 *
	 * @param managedType
	 * @return
	 */
	public static EntityManager getEntityManager(final Class<?> managedType)
	{
		final String persistenceUnit;
		if((persistenceUnit = getPersistenceUnit(managedType)) != null)
		{
			return getEntityManager(persistenceUnit);
		}
		
		return null;
	}
	
	
	/**
	 * 
	 * @param persistenceUnit
	 * @return
	 */
	public static EntityManager getEntityManager(final String persistenceUnit)
	{
		final Conversationables conversationables;
		if((conversationables = Conversationables.getCurrent()) != null)
		{
			final Conversationable conversationable;
			if((conversationable = conversationables.get(persistenceUnit)) != null)
			{
				return conversationable.getEntityManager();
			}
		}
		
		return null;
	}
	
	
	/**
	 * 
	 * @param managedType
	 * @return
	 */
	public static <T> CriteriaQuery<T> createCriteriaQuery(final Class<T> managedType)
	{
		final EntityManager entityManager;
		if((entityManager = getEntityManager(managedType)) != null)
		{
			return entityManager.getCriteriaBuilder().createQuery(managedType);
		}
		
		return null;
	}
	
	
	private Jpa()
	{
		throw new Error();
	}
}

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

package com.rapidclipse.framework.server.persistence.jpa.dal;

import java.io.Serializable;
import java.util.List;

import javax.persistence.criteria.CriteriaQuery;


/**
 * @author XDEV Software
 *
 * @param <T>
 * @param <ID>
 */
public interface DataAccessObject<T, ID extends Serializable>
{
	public void beginTransaction();
	
	public void rollback();
	
	public void commit();
	
	public CriteriaQuery<T> buildCriteriaQuery(Class<T> exampleType);
	
	public T find(ID id);
	
	public T[] find(@SuppressWarnings("unchecked") ID... ids);
	
	public List<T> findAll();
	
	public void flush();
	
	public T reattach(T object);
	
	public T getReference(ID id);
	
	public T[] getReferences(@SuppressWarnings("unchecked") ID... ids);
	
	public boolean isAttached(T entity);
	
	public void refresh(@SuppressWarnings("unchecked") T... entities);
	
	public boolean remove(T entity);
	
	public void remove(@SuppressWarnings("unchecked") T... entities);
	
	public boolean removeById(ID id);
	
	public void removeByIds(@SuppressWarnings("unchecked") ID... ids);
	
	public T merge(T entity);
	
	public T[] merge(@SuppressWarnings("unchecked") T... entities);
	
	public void persist(T entity);
	
	public void persist(@SuppressWarnings("unchecked") T... entities);
	
	public T save(T entity);
	
	public T[] save(@SuppressWarnings("unchecked") T... entities);
	
	public boolean contains(Object entity);
	
	/**
	 * Find and load a list of entities.
	 *
	 * @param entity
	 *            a sample entity whose non-null properties may be used as
	 *            search hints
	 * @return the entities matching the search.
	 */
	public List<T> findByExample(T entity);
	
	/**
	 * Find and load a list of entities.
	 *
	 * @param entity
	 *            a sample entity whose non-null properties may be used as
	 *            search hints
	 * @param searchParameters
	 *            carries additional search information
	 * @return the entities matching the search.
	 */
	public List<T> findByExample(T entity, SearchParameters searchParameters);
	
	/**
	 * Count the number of instances.
	 *
	 * @param entity
	 *            a sample entity whose non-null properties may be used as
	 *            search hint
	 * @return the number of entities matching the search
	 */
	public int countByExample(T entity);
	
	/**
	 * Count the number of instances.
	 *
	 * @param entity
	 *            a sample entity whose non-null properties may be used as
	 *            search hint
	 * @param searchParameters
	 *            carries additional search information
	 * @return the number of entities matching the search.
	 */
	public int countByExample(T entity, SearchParameters searchParameters);
}

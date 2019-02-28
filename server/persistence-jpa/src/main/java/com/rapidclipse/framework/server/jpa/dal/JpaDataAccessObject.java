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

package com.rapidclipse.framework.server.jpa.dal;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.List;
import java.util.Objects;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.Attribute;

import org.hibernate.LockMode;
import org.hibernate.LockOptions;
import org.hibernate.Session;

import com.rapidclipse.framework.server.jpa.Jpa;
import com.rapidclipse.framework.server.util.ReflectionUtils;


/**
 * Implementation of a <b>D</b>ata <b>A</b>ccess <b>O</b>bject using JPA that
 * can be used for a single specified type domain object.
 *
 * @author XDEV Software
 *
 * @param <T>
 *            The type of the domain object for which this instance is to be
 *            used.
 * @param <ID>
 *            The type of the id of the domain object for which this instance is
 *            to be used.
 */
public class JpaDataAccessObject<T, ID extends Serializable> implements DataAccessObject<T, ID>
{
	private final Class<T> persistentClass;
	
	public JpaDataAccessObject(final Class<T> persistentClass)
	{
		this.persistentClass = Objects.requireNonNull(persistentClass);
	}
	
	protected Class<T> persistentClass()
	{
		return this.persistentClass;
	}
	
	protected EntityManager em()
	{
		return Jpa.getEntityManager(this.persistentClass);
	}
	
	protected Attribute<?, ?> idAttribute()
	{
		Attribute<?, ?> idAttribute = Jpa.getIdAttribute(this.persistentClass);
		if(idAttribute == null)
		{
			idAttribute = Jpa.getEmbeddedIdAttribute(this.persistentClass);
		}
		return idAttribute;
	}
	
	@SuppressWarnings("unchecked")
	protected ID id(final T entity)
	{
		return (ID)ReflectionUtils.getMemberValue(entity, idAttribute().getJavaMember());
	}
	
	protected void applyCacheHints(final TypedQuery<?> typedQuery, final CacheableQuery.Kind kind)
	{
		Jpa.applyCacheHints(typedQuery, kind, this.persistentClass);
	}
	
	@Override
	public void beginTransaction()
	{
		em().getTransaction().begin();
	}
	
	@Override
	public void rollback()
	{
		em().getTransaction().rollback();
	}
	
	@Override
	public void commit()
	{
		em().getTransaction().commit();
	}
	
	@Override
	public CriteriaQuery<T> buildCriteriaQuery(final Class<T> exampleType)
	{
		final CriteriaBuilder cb = em().getCriteriaBuilder();
		return cb.createQuery(exampleType);
	}
	
	@Override
	public T find(final ID id)
	{
		return em().find(this.persistentClass, id);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T[] find(final ID... ids)
	{
		final T[] array = (T[])Array.newInstance(this.persistentClass, ids.length);
		for(int i = 0; i < ids.length; i++)
		{
			array[i] = find(ids[i]);
		}
		return array;
	}
	
	@Override
	public List<T> findAll()
	{
		final EntityManager    entityManager = em();
		final CriteriaQuery<T> criteriaQuery = entityManager.getCriteriaBuilder()
			.createQuery(this.persistentClass);
		criteriaQuery.from(this.persistentClass);
		final TypedQuery<T> typedQuery = entityManager.createQuery(criteriaQuery);
		applyCacheHints(typedQuery, CacheableQuery.Kind.FIND_ALL);
		return typedQuery.getResultList();
	}
	
	@Override
	public void flush()
	{
		em().flush();
	}
	
	@Override
	public T reattach(final T object)
	{
		final Session session = em().unwrap(Session.class);
		if(!session.contains(object))
		{
			session.lock(object, LockMode.NONE);
			session.refresh(object, new LockOptions(LockMode.NONE));
		}
		return object;
	}
	
	@Override
	public T getReference(final ID id)
	{
		return em().getReference(this.persistentClass, id);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T[] getReferences(final ID... ids)
	{
		final T[] array = (T[])Array.newInstance(this.persistentClass, ids.length);
		for(int i = 0; i < ids.length; i++)
		{
			array[i] = getReference(ids[i]);
		}
		return array;
	}
	
	@Override
	public boolean isAttached(final T entity)
	{
		return em().contains(entity);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void refresh(final T... entities)
	{
		final EntityManager entityManager = em();
		for(final T entity : entities)
		{
			entityManager.refresh(entity);
		}
	}
	
	@Override
	public boolean remove(final T entity)
	{
		final EntityManager entityManager = em();
		if(entity != null)
		{
			if(entityManager.contains(entity))
			{
				entityManager.remove(entity);
				return true;
			}
			else
			{
				return removeById(id(entity));
			}
		}
		return false;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void remove(final T... entities)
	{
		for(final T entity : entities)
		{
			remove(entity);
		}
	}
	
	@Override
	public boolean removeById(final ID id)
	{
		if(id != null)
		{
			final EntityManager    entityManager   = em();
			final CriteriaBuilder  criteriaBuilder = entityManager.getCriteriaBuilder();
			final CriteriaQuery<T> criteriaQuery   = criteriaBuilder
				.createQuery(this.persistentClass);
			final Root<T>          root            = criteriaQuery.from(this.persistentClass);
			criteriaQuery.where(criteriaBuilder.equal(root.get(idAttribute().getName()), id));
			final TypedQuery<T> typedQuery = entityManager.createQuery(criteriaQuery);
			applyCacheHints(typedQuery, CacheableQuery.Kind.REMOVE_BY_ID);
			if(!typedQuery.getResultList().isEmpty())
			{
				entityManager.remove(entityManager.getReference(this.persistentClass, id));
				return true;
			}
		}
		return false;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void removeByIds(final ID... ids)
	{
		for(final ID id : ids)
		{
			removeById(id);
		}
	}
	
	@Override
	public T merge(final T entity)
	{
		return em().merge(entity);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T[] merge(final T... entities)
	{
		final T[] array = (T[])Array.newInstance(this.persistentClass, entities.length);
		for(int i = 0; i < entities.length; i++)
		{
			array[i] = merge(entities[i]);
		}
		return array;
	}
	
	@Override
	public final void persist(final T entity)
	{
		em().persist(entity);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public final void persist(final T... entities)
	{
		for(final T entity : entities)
		{
			persist(entity);
		}
	}
	
	@Override
	public T save(final T entity)
	{
		if(entity == null)
		{
			return null;
		}
		if(em().contains(entity))
		{
			return entity;
		}
		final ID id = id(entity);
		if(!validId(id))
		{
			persist(entity);
			return entity;
		}
		final T prev = em().find(this.persistentClass, id);
		if(prev == null)
		{
			persist(entity);
			return entity;
		}
		else
		{
			return merge(entity);
		}
	}
	
	private boolean validId(final Serializable id)
	{
		if(id == null)
		{
			return false;
		}
		if(id instanceof Number && ((Number)id).equals(0))
		{
			return false;
		}
		if(id instanceof String && "".equals(id))
		{
			return false;
		}
		return true;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public T[] save(final T... entities)
	{
		final T[] array = (T[])Array.newInstance(this.persistentClass, entities.length);
		for(int i = 0; i < entities.length; i++)
		{
			array[i] = save(entities[i]);
		}
		return array;
	}
	
	@Override
	public boolean contains(final Object entity)
	{
		return em().contains(entity);
	}
	
	@Override
	public List<T> findByExample(final T entity)
	{
		return findByExample(entity, new SearchParameters());
	}
	
	@Override
	public List<T> findByExample(final T entity, final SearchParameters searchParameters)
	{
		return new FindByExample<T>(this.persistentClass, em(), searchParameters)
			.findByExample(entity);
	}
	
	@Override
	public int countByExample(final T entity)
	{
		return countByExample(entity, new SearchParameters());
	}
	
	@Override
	public int countByExample(final T entity, final SearchParameters searchParameters)
	{
		return new FindByExample<T>(this.persistentClass, em(), searchParameters)
			.countByExample(entity);
	}
}
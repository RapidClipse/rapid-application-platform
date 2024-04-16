/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.jpa.dal;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.metamodel.Attribute;

import org.hibernate.LockMode;
import org.hibernate.LockOptions;
import org.hibernate.Session;

import com.rapidclipse.framework.server.data.DataAccessObject;
import com.rapidclipse.framework.server.jpa.Jpa;
import com.rapidclipse.framework.server.util.ReflectionUtils;


/**
 * A data access object using JPA that
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
public interface JpaDataAccessObject<T, ID extends Serializable> extends DataAccessObject<T>
{
	public void beginTransaction();

	public void rollback();

	public void commit();

	public CriteriaQuery<T> buildCriteriaQuery(Class<T> exampleType);

	public T find(ID id);

	public T[] find(@SuppressWarnings("unchecked") ID... ids);

	public List<T> findAll();

	public CriteriaQuery<T> findAllQuery();

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
	
	public static class Default<T, ID extends Serializable> implements JpaDataAccessObject<T, ID>
	{
		private final Class<T> persistentClass;

		protected Default(final Class<T> persistentClass)
		{
			this.persistentClass = requireNonNull(persistentClass);
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
			final TypedQuery<T> typedQuery = em().createQuery(findAllQuery());
			applyCacheHints(typedQuery, CacheableQuery.Kind.FIND_ALL);
			return typedQuery.getResultList();
		}
		
		@Override
		public CriteriaQuery<T> findAllQuery()
		{
			final CriteriaQuery<T> criteriaQuery = em().getCriteriaBuilder()
				.createQuery(this.persistentClass);
			criteriaQuery.from(this.persistentClass);
			return criteriaQuery;
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
			return findByExample(entity, SearchParameters.New());
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
			return countByExample(entity, SearchParameters.New());
		}

		@Override
		public int countByExample(final T entity, final SearchParameters searchParameters)
		{
			return new FindByExample<T>(this.persistentClass, em(), searchParameters)
				.countByExample(entity);
		}
	}
}

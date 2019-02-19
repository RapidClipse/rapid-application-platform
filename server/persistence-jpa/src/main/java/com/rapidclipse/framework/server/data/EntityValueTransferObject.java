/*-
 * ---
 * Rapid Application Platform / Server / Persistence / JPA
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.data;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;


/**
 * @author XDEV Software
 *
 */
public class EntityValueTransferObject implements Serializable
{
	private final Class<?>     entityType;
	private final Class<?>     idType;
	private final Serializable id;
	
	public EntityValueTransferObject(final Class<?> entityType, final Serializable id)
	{
		super();
		this.entityType = requireNonNull(entityType);
		this.idType     = id.getClass();
		this.id         = id;
	}
	
	public Class<?> entityType()
	{
		return this.entityType;
	}
	
	public Class<?> idType()
	{
		return this.idType;
	}
	
	public Serializable id()
	{
		return this.id;
	}
	
	@Override
	public boolean equals(final Object obj)
	{
		if(obj == this)
		{
			return true;
		}
		
		if(!(obj instanceof EntityValueTransferObject))
		{
			return false;
		}
		
		final EntityValueTransferObject other = (EntityValueTransferObject)obj;
		return this.entityType.equals(other.entityType) && this.idType.equals(other.idType)
			&& this.id.equals(other.id);
	}
}

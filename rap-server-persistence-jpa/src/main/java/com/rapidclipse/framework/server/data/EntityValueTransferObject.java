/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

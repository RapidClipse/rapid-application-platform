/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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

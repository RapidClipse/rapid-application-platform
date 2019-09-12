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

import java.io.Serializable;

import com.rapidclipse.framework.server.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public class EntityValueTransferHandler implements ValueTransferHandler
{
	public EntityValueTransferHandler()
	{
		super();
	}
	
	@Override
	public boolean handlesPut(final Object value)
	{
		return value != null && Jpa.isManaged(value.getClass())
			&& Jpa.getEntityIdAttributeValue(value) instanceof Serializable;
	}
	
	@Override
	public Object put(final Object value)
	{
		return new EntityValueTransferObject(value.getClass(),
			(Serializable)Jpa.getEntityIdAttributeValue(value));
	}
	
	@Override
	public boolean handlesGet(final Object value)
	{
		return value instanceof EntityValueTransferObject;
	}
	
	@Override
	public Object get(final Object value)
	{
		final EntityValueTransferObject transferObject = (EntityValueTransferObject)value;
		
		Serializable   id     = transferObject.id();
		final Class<?> idType = transferObject.idType();
		if(!idType.isAssignableFrom(id.getClass()) && id instanceof Number)
		{
			/*
			 * Ensure correct number type, may have changed due to serialization
			 * framework's inadequacies.
			 */
			if(idType == Integer.class)
			{
				id = ((Number)id).intValue();
			}
			else if(idType == Short.class)
			{
				id = ((Number)id).shortValue();
			}
			else if(idType == Byte.class)
			{
				id = ((Number)id).byteValue();
			}
			else if(idType == Long.class)
			{
				id = ((Number)id).longValue();
			}
			else if(idType == Float.class)
			{
				id = ((Number)id).floatValue();
			}
		}
		
		return Jpa.getDaoByEntityType(transferObject.entityType()).find(id);
	}
}

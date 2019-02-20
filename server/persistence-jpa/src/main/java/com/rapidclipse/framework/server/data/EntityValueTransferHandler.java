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

package com.rapidclipse.framework.server.data;

import java.io.Serializable;

import com.rapidclipse.framework.server.persistence.jpa.Jpa;


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
		
		Serializable                    id             = transferObject.id();
		final Class<?>                  idType         = transferObject.idType();
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

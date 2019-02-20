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

package com.rapidclipse.framework.server.resources;

import com.rapidclipse.framework.server.persistence.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public class EntityClassCaptionResolverFactory implements ClassCaptionResolverFactory
{
	private ClassCaptionResolver classCaptionResolver;
	
	public EntityClassCaptionResolverFactory()
	{
		super();
	}
	
	@Override
	public ClassCaptionResolver getClassCaptionResolver(final Class<?> clazz)
	{
		if(Jpa.isManaged(clazz))
		{
			if(this.classCaptionResolver == null)
			{
				this.classCaptionResolver = new EntityClassCaptionResolver();
			}
			
			return this.classCaptionResolver;
		}
		
		return null;
	}
}

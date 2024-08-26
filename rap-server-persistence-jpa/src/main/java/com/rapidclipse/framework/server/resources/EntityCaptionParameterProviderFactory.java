/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.resources;

import com.rapidclipse.framework.server.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public class EntityCaptionParameterProviderFactory implements CaptionParameterProviderFactory
{
	private CaptionParameterProvider captionParameterProvider;
	
	public EntityCaptionParameterProviderFactory()
	{
		super();
	}
	
	@Override
	public CaptionParameterProvider getParameterProvider(final Object element)
	{
		if(Jpa.isManaged(element.getClass()))
		{
			if(this.captionParameterProvider == null)
			{
				this.captionParameterProvider = new EntityCaptionParameterProvider();
			}
			
			return this.captionParameterProvider;
		}
		
		return null;
	}
}

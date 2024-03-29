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

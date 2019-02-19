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

package com.rapidclipse.framework.server.resources;

import com.rapidclipse.framework.server.persistence.jpa.Jpa;


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

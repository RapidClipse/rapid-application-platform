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
package com.rapidclipse.framework.server.resources;

import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public class EntityClassCaptionResolver implements ClassCaptionResolver
{
	public EntityClassCaptionResolver()
	{
		super();
	}
	
	@Override
	public String resolveCaption(final Class<?> clazz, final String propertyName)
	{
		final Attribute<?, ?> attribute = Jpa.resolveAttribute(clazz, propertyName);
		if(attribute != null)
		{
			final Member javaMember = attribute.getJavaMember();
			if(javaMember instanceof AnnotatedElement
				&& CaptionUtils.hasCaptionAnnotationValue((AnnotatedElement)javaMember))
			{
				return CaptionUtils.resolveCaption(javaMember);
			}
			
			return attribute.getName();
		}
		
		return null;
	}
}

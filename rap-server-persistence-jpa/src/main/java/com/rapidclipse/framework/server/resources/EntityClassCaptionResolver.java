/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
		Attribute<?, ?> attribute = null;
		try
		{
			attribute = Jpa.resolveAttribute(clazz, propertyName);
		}
		catch(final IllegalArgumentException e)
		{
			// swallow, probably a transient field, let other CaptionResolver take over
		}
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

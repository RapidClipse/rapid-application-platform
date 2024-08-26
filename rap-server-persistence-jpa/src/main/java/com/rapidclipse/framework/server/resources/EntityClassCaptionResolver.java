/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.resources;

import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;

import jakarta.persistence.metamodel.Attribute;

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

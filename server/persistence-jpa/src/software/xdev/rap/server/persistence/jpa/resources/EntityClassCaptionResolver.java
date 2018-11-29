/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.persistence.jpa.resources;


import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;

import javax.persistence.metamodel.Attribute;

import software.xdev.rap.server.persistence.jpa.Jpa;
import software.xdev.rap.server.resources.CaptionUtils;
import software.xdev.rap.server.resources.ClassCaptionResolver;


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
		final Attribute<?, ?> attribute = Jpa.resolveAttribute(clazz,propertyName);
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

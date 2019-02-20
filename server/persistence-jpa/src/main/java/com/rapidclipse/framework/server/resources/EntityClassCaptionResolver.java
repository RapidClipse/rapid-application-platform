
package com.rapidclipse.framework.server.resources;

import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.persistence.jpa.Jpa;


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

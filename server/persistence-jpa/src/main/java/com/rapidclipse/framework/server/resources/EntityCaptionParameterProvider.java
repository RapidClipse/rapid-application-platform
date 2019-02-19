
package com.rapidclipse.framework.server.resources;

import com.rapidclipse.framework.server.persistence.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public class EntityCaptionParameterProvider implements CaptionParameterProvider
{
	public EntityCaptionParameterProvider()
	{
		super();
	}
	
	@Override
	public String getParameterValue(final Object element, final String parameterName)
	{
		return String.valueOf(Jpa.resolveValue(element, parameterName));
	}
}

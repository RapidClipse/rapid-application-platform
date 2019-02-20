
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

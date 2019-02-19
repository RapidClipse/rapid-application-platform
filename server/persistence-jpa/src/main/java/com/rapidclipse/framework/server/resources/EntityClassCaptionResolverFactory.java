
package com.rapidclipse.framework.server.resources;

import com.rapidclipse.framework.server.persistence.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public class EntityClassCaptionResolverFactory implements ClassCaptionResolverFactory
{
	private ClassCaptionResolver classCaptionResolver;
	
	public EntityClassCaptionResolverFactory()
	{
		super();
	}
	
	@Override
	public ClassCaptionResolver getClassCaptionResolver(final Class<?> clazz)
	{
		if(Jpa.isManaged(clazz))
		{
			if(this.classCaptionResolver == null)
			{
				this.classCaptionResolver = new EntityClassCaptionResolver();
			}

			return this.classCaptionResolver;
		}

		return null;
	}
}

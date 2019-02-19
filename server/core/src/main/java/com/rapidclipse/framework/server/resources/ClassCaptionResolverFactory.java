
package com.rapidclipse.framework.server.resources;

import com.rapidclipse.framework.server.util.ServicePriority;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface ClassCaptionResolverFactory
{
	public ClassCaptionResolver getClassCaptionResolver(Class<?> clazz);
	
	@ServicePriority(ServicePriority.MIN)
	public static class BeanClassCaptionResolverFactory implements ClassCaptionResolverFactory
	{
		private ClassCaptionResolver classCaptionResolver;
		
		public BeanClassCaptionResolverFactory()
		{
			super();
		}
		
		@Override
		public ClassCaptionResolver getClassCaptionResolver(final Class<?> clazz)
		{
			if(this.classCaptionResolver == null)
			{
				this.classCaptionResolver = new ClassCaptionResolver.BeanClassCaptionResolver();
			}

			return this.classCaptionResolver;
		}
	}
}

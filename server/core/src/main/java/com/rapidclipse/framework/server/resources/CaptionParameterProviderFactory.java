
package com.rapidclipse.framework.server.resources;

import java.lang.reflect.Member;

import com.rapidclipse.framework.server.util.ServicePriority;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface CaptionParameterProviderFactory
{
	public CaptionParameterProvider getParameterProvider(final Object element);
	
	public static class MemberCaptionParameterProviderFactory
		implements CaptionParameterProviderFactory
	{
		private CaptionParameterProvider captionParameterProvider;
		
		@Override
		public CaptionParameterProvider getParameterProvider(final Object element)
		{
			if(element instanceof Member)
			{
				if(this.captionParameterProvider == null)
				{
					this.captionParameterProvider = new CaptionParameterProvider.SimpleCaptionParameterProvider();
				}

				return this.captionParameterProvider;
			}

			return null;
		}
	}
	
	@ServicePriority(ServicePriority.MIN)
	public static class BeanInfoCaptionParameterProviderFactory
		implements CaptionParameterProviderFactory
	{
		private CaptionParameterProvider captionParameterProvider;
		
		@Override
		public CaptionParameterProvider getParameterProvider(final Object element)
		{
			if(this.captionParameterProvider == null)
			{
				this.captionParameterProvider = new CaptionParameterProvider.BeanInfoCaptionParameterProvider();
			}
			
			return this.captionParameterProvider;
		}
	}
}

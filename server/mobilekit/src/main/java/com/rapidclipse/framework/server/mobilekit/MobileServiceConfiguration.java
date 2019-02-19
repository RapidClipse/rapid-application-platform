
package com.rapidclipse.framework.server.mobilekit;

import java.util.Map;


/**
 * @author XDEV Software
 *
 */
public interface MobileServiceConfiguration
{
	public Class<? extends MobileService> getServiceClass();
	
	public Map<String, String> getParameters();
	
	public static MobileServiceConfiguration New(
		final Class<? extends MobileService> serviceClass,
		final Map<String, String> params)
	{
		return new Implementation(serviceClass, params);
	}
	
	public static class Implementation implements MobileServiceConfiguration
	{
		private final Class<? extends MobileService> serviceClass;
		private final Map<String, String>            params;
		
		public Implementation(
			final Class<? extends MobileService> serviceClass,
			final Map<String, String> params)
		{
			super();
			
			this.serviceClass = serviceClass;
			this.params       = params;
		}
		
		@Override
		public Class<? extends MobileService> getServiceClass()
		{
			return this.serviceClass;
		}
		
		@Override
		public Map<String, String> getParameters()
		{
			return this.params;
		}
	}
}


package com.rapidclipse.framework.server.mobilekit;

import java.util.Collection;


/**
 * @author XDEV Software
 *
 */
public interface MobileConfiguration
{
	public Collection<MobileServiceConfiguration> getMobileServices();
	
	public static MobileConfiguration New(
		final Collection<MobileServiceConfiguration> mobileServices)
	{
		return new Implementation(mobileServices);
	}
	
	public static class Implementation implements MobileConfiguration
	{
		private final Collection<MobileServiceConfiguration> mobileServices;
		
		public Implementation(final Collection<MobileServiceConfiguration> mobileServices)
		{
			super();
			
			this.mobileServices = mobileServices;
		}
		
		@Override
		public Collection<MobileServiceConfiguration> getMobileServices()
		{
			return this.mobileServices;
		}
	}
}

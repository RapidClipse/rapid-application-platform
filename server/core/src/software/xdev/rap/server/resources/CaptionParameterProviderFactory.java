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

package software.xdev.rap.server.resources;


import java.lang.reflect.Member;

import software.xdev.rap.server.util.ServicePriority;


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

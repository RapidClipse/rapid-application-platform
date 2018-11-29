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


import software.xdev.rap.server.util.ServicePriority;


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

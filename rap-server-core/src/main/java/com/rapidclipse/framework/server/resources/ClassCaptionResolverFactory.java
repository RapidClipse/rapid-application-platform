/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
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

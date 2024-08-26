/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
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

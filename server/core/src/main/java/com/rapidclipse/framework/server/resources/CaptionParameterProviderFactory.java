/*-
 * ---
 * Rapid Application Platform / Server / Core
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
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

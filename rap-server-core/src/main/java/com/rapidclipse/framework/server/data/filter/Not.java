/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.filter;

/**
 * @author XDEV Software
 *
 */
public interface Not extends Filter
{
	public Filter filter();
	
	public static Not New(final Filter filter)
	{
		return new Default(filter);
	}
	
	public static class Default implements Not
	{
		private final Filter filter;
		
		protected Default(final Filter filter)
		{
			super();
			this.filter = filter;
		}
		
		@Override
		public Filter filter()
		{
			return this.filter;
		}
	}
}

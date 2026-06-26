/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Bar extends Serializable, JavaScriptable
{
	public Size groupWidth();
	
	public static Bar New(final Size groupWidth)
	{
		return new Default(groupWidth);
	}
	
	public static class Default implements Bar
	{
		private final Size groupWidth;
		
		Default(final Size groupWidth)
		{
			super();
			
			this.groupWidth = groupWidth;
		}
		
		@Override
		public Size groupWidth()
		{
			return this.groupWidth;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("groupWidth", this.groupWidth);
			return obj.js();
		}
	}
}

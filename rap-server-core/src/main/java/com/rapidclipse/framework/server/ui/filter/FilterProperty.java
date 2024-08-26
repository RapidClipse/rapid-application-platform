/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.filter;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;


public interface FilterProperty<T> extends Serializable
{
	public Object identifier();
	
	public Class<T> type();
	
	public String caption();
	
	public static <T> FilterProperty<T> New(
		final Object identifier,
		final Class<T> type,
		final String caption)
	{
		return new Default<>(identifier, type, caption);
	}
	
	public static class Default<T> implements FilterProperty<T>
	{
		private final Object   identifier;
		private final Class<T> type;
		private final String   caption;
		
		protected Default(final Object identifier, final Class<T> type, final String caption)
		{
			super();
			
			this.identifier = requireNonNull(identifier);
			this.type       = requireNonNull(type);
			this.caption    = requireNonNull(caption);
		}
		
		@Override
		public Object identifier()
		{
			return this.identifier;
		}
		
		@Override
		public Class<T> type()
		{
			return this.type;
		}
		
		@Override
		public String caption()
		{
			return this.caption;
		}
	}
}

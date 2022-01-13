/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface AnnotationStyle extends Serializable, JavaScriptable
{
	public Stem stem();
	
	public String style();
	
	public static AnnotationStyle New(final Stem stem)
	{
		return new Default(stem, null);
	}
	
	public static AnnotationStyle New(final String style)
	{
		return new Default(null, style);
	}
	
	public static AnnotationStyle New(final Stem stem, final String style)
	{
		return new Default(stem, style);
	}
	
	public static class Default implements AnnotationStyle
	{
		private final Stem   stem;
		private final String style;
		
		Default(final Stem stem, final String style)
		{
			super();
			
			this.stem  = stem;
			this.style = style;
		}
		
		@Override
		public Stem stem()
		{
			return this.stem;
		}
		
		@Override
		public String style()
		{
			return this.style;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stem", this.stem);
			obj.putIfNotNull("style", this.style);
			return obj.js();
		}
	}
}

/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.maps;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Icon extends Serializable, JavaScriptable
{
	public String normal();

	public String selected();

	public static Icon New(final String normal, final String selected)
	{
		return new Default(normal, selected);
	}
	
	public static class Default implements Icon
	{
		private final String normal;
		private final String selected;

		Default(final String normal, final String selected)
		{
			super();

			this.normal   = normal;
			this.selected = selected;
		}

		@Override
		public String normal()
		{
			return this.normal;
		}

		@Override
		public String selected()
		{
			return this.selected;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("normal", this.normal);
			obj.putIfNotNull("selected", this.selected);
			return obj.js();
		}
	}
}

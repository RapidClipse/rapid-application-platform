/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.charts.sankey;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Sankey extends Serializable, JavaScriptable
{
	public Link link();
	
	public Node node();

	public static Sankey New(final Link link, final Node node)
	{
		return new Default(link, node);
	}
	
	public static class Default implements Sankey
	{
		private final Link link;
		private final Node node;
		
		Default(final Link link, final Node node)
		{
			super();

			this.link = link;
			this.node = node;
		}

		@Override
		public Link link()
		{
			return this.link;
		}
		
		@Override
		public Node node()
		{
			return this.node;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("link", this.link);
			obj.putIfNotNull("node", this.node);
			return obj.js();
		}
		
	}
	
}

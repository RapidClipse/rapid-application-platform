/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

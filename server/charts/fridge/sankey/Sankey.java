/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts.sankey;

/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Sankey
{
	private Integer iterations = 32;
	private Link    link;
	private Node    node;

	public Integer getIterations()
	{
		return this.iterations;
	}

	/**
	 * The larger this number, the more pleasing the layout of complex sankeys
	 * Default: 32
	 *
	 * @param iterations
	 */
	public void setIterations(final Integer iterations)
	{
		this.iterations = iterations;
	}

	public Link getLink()
	{
		return this.link;
	}

	/**
	 * Controls attributes of the connections between nodes.
	 *
	 * @param link
	 */
	public void setLink(final Link link)
	{
		this.link = link;
	}

	public Node getNode()
	{
		return this.node;
	}

	/**
	 * Controls attributes of the nodes (the vertical bars between links):
	 *
	 * @param node
	 */
	public void setNode(final Node node)
	{
		this.node = node;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		str.append("iterations: " + this.iterations + ", ");
		str.append("link: " + this.link + ", ");
		str.append("node: " + this.node + " ");
		str.append("}");
		
		return str.toString();
	}
}

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

import java.util.List;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Node
{
	private Integer      labelPadding = 6;
	private Integer      nodePadding  = 10;
	private Integer      width        = 5;
	private List<String> colors;

	public Integer getLabelPadding()
	{
		return this.labelPadding;
	}
	
	/**
	 * Horizontal distance between the label and the node.
	 * Default: 6
	 *
	 * @param labelPadding
	 */
	public void setLabelPadding(final Integer labelPadding)
	{
		this.labelPadding = labelPadding;
	}
	
	public Integer getNodePadding()
	{
		return this.nodePadding;
	}
	
	/**
	 * Vertical distance between nodes.
	 * Default: 10
	 *
	 * @param nodePadding
	 */
	public void setNodePadding(final Integer nodePadding)
	{
		this.nodePadding = nodePadding;
	}
	
	public Integer getWidth()
	{
		return this.width;
	}
	
	/**
	 * Thickness of the node.
	 * Default: 5
	 *
	 * @param width
	 */
	public void setWidth(final Integer width)
	{
		this.width = width;
	}
	
	public List<String> getColors()
	{
		return this.colors;
	}
	
	/**
	 * Custom color palette for sankey nodes.
	 * Nodes will cycle through this palette.
	 *
	 * @param colors
	 */
	public void setColors(final List<String> colors)
	{
		this.colors = colors;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("labelPadding: " + this.labelPadding + ", ");
		str.append("nodePadding: " + this.nodePadding + ", ");
		if(this.colors != null)
		{
			str.append("colors: [");
			for(final String s : this.colors)
			{
				str.append("'" + s + "',");
			}
			str.delete(str.length() - 1, str.length());
			str.append("], ");
		}
		str.append("width: " + this.width + " ");
		str.append("}");

		return str.toString();
	}
}

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
public class Link
{
	private String colorMode = "none";

	public String getColorMode()
	{
		return this.colorMode;
	}

	/**
	 * Sets a coloring mode for the links between nodes. Possible values:
	 *
	 * <li>'source' - The color of the source node is used for the links to all target nodes.</li>
	 * <li>'target' - The color of the target node is used for the link to its source nodes.</li>
	 * <li>'gradient' - The link between a source and target node is colored as a gradient from the source node color to
	 * the target node color.</li>
	 * <li>'none' - the default option; link colors will be set to the default</li>
	 *
	 * @param colorMode
	 */
	public void setColorMode(final String colorMode)
	{
		this.colorMode = colorMode;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		
		str.append("colorMode: '" + this.colorMode + "' ");

		str.append("}");

		return str.toString();
	}

}

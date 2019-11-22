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

package com.rapidclipse.framework.server.charts.config;

/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class BarColumnDiff
{
	private double widthFactor = 0.3;
	private String oldColor;
	
	public double getWidthFactor()
	{
		return this.widthFactor;
	}

	/**
	 * for Barchart or Columnchart
	 * Sets the relative width of the new data bar.
	 * 0.0-1.0
	 * Default: 0.3
	 *
	 * @param widthFactor
	 */
	public void setWidthFactor(final double widthFactor)
	{
		this.widthFactor = widthFactor;
	}

	public String getOldColor()
	{
		return this.oldColor;
	}

	public void setOldColor(final String oldColor)
	{
		this.oldColor = oldColor;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		if(this.oldColor != null)
		{
			str.append("oldData: { color: '" + this.oldColor + "'}, ");
		}
		str.append("newData: { widthFactor: " + this.widthFactor + "} ");
		str.append("}");
		
		return str.toString();
	}
}

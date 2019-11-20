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
 * @author XDEV Software
 * @since 10.02.00
 */
public class PieDiff
{
	private double  radiusFactor = 0.5;
	private double  borderFactor = 0.02;
	private double  oldOpacity   = 0.5;
	private double  newOpacity   = 1.0;
	private boolean inCenter     = true;
	
	public double getRadiusFactor()
	{
		return this.radiusFactor;
	}
	
	/**
	 * for Piechart
	 * How big the inner circle should be displayed.
	 *
	 * @param radiusFactor
	 */
	public void setRadiusFactor(final double radiusFactor)
	{
		this.radiusFactor = radiusFactor;
	}

	public double getBorderFactor()
	{
		return this.borderFactor;
	}

	/**
	 * for Piechart
	 * Thickness of the border.
	 *
	 * @param borderFactor
	 */
	public void setBorderFactor(final double borderFactor)
	{
		this.borderFactor = borderFactor;
	}

	public double getOldOpacity()
	{
		return this.oldOpacity;
	}

	/**
	 * for Piechart
	 * Opacity of the old data.
	 *
	 * @param oldOpacity
	 */
	public void setOldOpacity(final double oldOpacity)
	{
		this.oldOpacity = oldOpacity;
	}

	public double getNewOpacity()
	{
		return this.newOpacity;
	}

	/**
	 * for Piechart
	 * Opacity of the new data.
	 *
	 * @param newOpacity
	 */
	public void setNewOpacity(final double newOpacity)
	{
		this.newOpacity = newOpacity;
	}

	public boolean isInCenter()
	{
		return this.inCenter;
	}

	/**
	 * for Piechart
	 * Switch the old data to the outside.
	 *
	 * @param inCenter
	 */
	public void setInCenter(final boolean inCenter)
	{
		this.inCenter = inCenter;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		str.append("innerCircle: { radiusFactor: " + this.radiusFactor + ", ");
		str.append("borderFactor: " + this.borderFactor + "}, ");
		str.append("oldData: { opacity: " + this.oldOpacity + ", ");
		str.append("inCenter: " + this.inCenter + "}, ");
		str.append("newData: { opacity: " + this.newOpacity + "} ");
		
		str.append("}");
		
		return str.toString();
	}
	
}

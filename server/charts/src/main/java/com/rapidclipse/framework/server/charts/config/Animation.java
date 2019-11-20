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
public class Animation
{
	private Integer duration = 200;
	private boolean startup  = true;
	private String  easing   = "linear";

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("duration: " + this.duration + ", ");
		str.append("easing: '" + this.easing + "', ");
		str.append("startup: " + this.startup);
		str.append("}");
		return str.toString();
	}

	public Integer getDuration()
	{
		return this.duration;
	}

	public boolean isStartup()
	{
		return this.startup;
	}

	public String getEasing()
	{
		return this.easing;
	}
	
	/**
	 * The duration of the animation, in milliseconds.
	 *
	 * @param duration
	 */
	public void setDuration(final Integer duration)
	{
		this.duration = duration;
	}
	
	/**
	 * Determines if the chart will animate on the initial draw. If true, the chart will start at the baseline and
	 * animate to its final state.
	 *
	 * @param startup
	 */
	public void setStartup(final boolean startup)
	{
		this.startup = startup;
	}
	
	/**
	 * *
	 * <li>'linear' - Constant speed.</li>
	 * <li>'in' - Ease in - Start slow and speed up.</li>
	 * <li>'out' - Ease out - Start fast and slow down.</li>
	 * <li>'inAndOut' - Ease in and out - Start slow, speed up, then slow down.</li>
	 *
	 * @param easing
	 */
	public void setEasing(final String easing)
	{
		this.easing = easing;
	}
	
}

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

package com.rapidclipse.framework.server.charts.gauge;

import java.util.HashMap;

import com.rapidclipse.framework.server.charts.config.Animation;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class GaugeChartConfig
{
	
	private Integer   greenFrom;
	private Integer   greenTo;
	private Integer   max        = 100;
	private Integer   min        = 0;
	private Integer   redFrom    = 85;
	private Integer   redTo      = 100;
	private Integer   yellowFrom = 60;
	private Integer   yellowTo   = 85;
	private Animation animation  = new Animation();
	
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("greenFrom", this.greenFrom);
		options.put("greenTo", this.greenTo);
		options.put("max", this.max);
		options.put("min", this.min);
		options.put("redFrom", this.redFrom);
		options.put("redTo", this.redTo);
		options.put("yellowFrom", this.yellowFrom);
		options.put("yellowTo", this.yellowTo);
		options.put("animation", this.animation);
		return options;
	}

	public Integer getGreenFrom()
	{
		return this.greenFrom;
	}
	
	/**
	 * The lowest value for a range marked by a green color.
	 *
	 * @param greenFrom
	 */
	public void setGreenFrom(final Integer greenFrom)
	{
		this.greenFrom = greenFrom;
	}

	public Integer getGreenTo()
	{
		return this.greenTo;
	}

	/**
	 * The highest value for a range marked by a green color.
	 *
	 * @param greenTo
	 */
	public void setGreenTo(final Integer greenTo)
	{
		this.greenTo = greenTo;
	}

	public Integer getMax()
	{
		return this.max;
	}

	/**
	 * The maximal value of a gauge.
	 *
	 * @param max
	 */
	public void setMax(final Integer max)
	{
		this.max = max;
	}

	public Integer getMin()
	{
		return this.min;
	}

	/**
	 * The minimal value of a gauge.
	 *
	 * @param min
	 */
	public void setMin(final Integer min)
	{
		this.min = min;
	}

	public Integer getRedFrom()
	{
		return this.redFrom;
	}

	/**
	 * The lowest value for a range marked by a red color.
	 *
	 * @param redFrom
	 */
	public void setRedFrom(final Integer redFrom)
	{
		this.redFrom = redFrom;
	}

	public Integer getRedTo()
	{
		return this.redTo;
	}

	/**
	 * The highest value for a range marked by a red color.
	 *
	 * @param redTo
	 */
	public void setRedTo(final Integer redTo)
	{
		this.redTo = redTo;
	}

	public Integer getYellowFrom()
	{
		return this.yellowFrom;
	}

	/**
	 * The lowest value for a range marked by a yellow color.
	 *
	 * @param yellowFrom
	 */
	public void setYellowFrom(final Integer yellowFrom)
	{
		this.yellowFrom = yellowFrom;
	}

	public Integer getYellowTo()
	{
		return this.yellowTo;
	}

	/**
	 * The highest value for a range marked by a yellow color.
	 *
	 * @param yellowTo
	 */
	public void setYellowTo(final Integer yellowTo)
	{
		this.yellowTo = yellowTo;
	}

	public Animation getAnimation()
	{
		return this.animation;
	}

	/**
	 * Animation options
	 *
	 * @param animation
	 */
	public void setAnimation(final Animation animation)
	{
		this.animation = animation;
	}

}

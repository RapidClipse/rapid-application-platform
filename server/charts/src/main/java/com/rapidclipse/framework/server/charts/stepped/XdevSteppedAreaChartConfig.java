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
package com.rapidclipse.framework.server.charts.stepped;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.VAxis;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevSteppedAreaChartConfig extends AbstractXdevChartConfig implements Serializable
{

	private VAxis   vAxis;
	private HAxis   hAxis;
	private boolean isStacked     = true;
	private String  titlePosition = "out";
	
	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("hAxis", this.hAxis);
		options.put("vAxis", this.vAxis);
		options.put("isStacked", this.isStacked);
		options.put("titlePosition", this.titlePosition);
		
		return options;
	}
	
	public VAxis getvAxis()
	{
		return this.vAxis;
	}

	public void setvAxis(final VAxis vAxis)
	{
		this.vAxis = vAxis;
	}

	public HAxis gethAxis()
	{
		return this.hAxis;
	}

	public void sethAxis(final HAxis hAxis)
	{
		this.hAxis = hAxis;
	}

	public boolean isStacked()
	{
		return this.isStacked;
	}

	public void setStacked(final boolean isStacked)
	{
		this.isStacked = isStacked;
	}

	public String getTitlePosition()
	{
		return this.titlePosition;
	}

	public void setTitlePosition(final String titlePosition)
	{
		this.titlePosition = titlePosition;
	}

}

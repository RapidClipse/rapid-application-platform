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

package com.rapidclipse.framework.server.charts.bar;

import java.util.LinkedHashMap;
import java.util.Map;

import com.rapidclipse.framework.server.charts.Axis;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("column-chart")
public class ColumnChart extends AbstractBarChart<ColumnChart>
{
	private final Map<Integer, Axis> vAxes = new LinkedHashMap<>();
	
	public ColumnChart()
	{
		super("ColumnChart");
	}
	
	public ColumnChart addVAxis(final int rowIndex, final Axis axis)
	{
		this.vAxes.put(rowIndex, axis);
		return this;
	}
	
	public Axis removeVAxis(final int rowIndex)
	{
		return this.vAxes.remove(rowIndex);
	}
	
	public ColumnChart removeAllVAxes()
	{
		this.vAxes.clear();
		return this;
	}
	
	@Override
	protected void createConfiguration(final ObjectHelper obj)
	{
		super.createConfiguration(obj);
		
		putIfNotNull(obj, "hAxes", this.vAxes);
	}
}

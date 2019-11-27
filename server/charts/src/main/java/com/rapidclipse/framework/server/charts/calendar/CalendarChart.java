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

package com.rapidclipse.framework.server.charts.calendar;

import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("calendar-chart")
public class CalendarChart extends Chart
{
	private Calendar      calendar;
	private ColorAxis     colorAxis;
	private Boolean       forceIFrame;
	private NoDataPattern noDataPattern;

	public CalendarChart()
	{
		super("Calendar", "calendar");
	}

	public ChartModel
		initDefaultColumns(final String dateColumn, final Column.Type dateColumnType, final String valueColumn)
	{
		return getModel().removeAll()
			.addColumn(Column.New(dateColumnType, dateColumn))
			.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
	}

	public Calendar getCalendar()
	{
		return this.calendar;
	}

	public void setCalendar(final Calendar calendar)
	{
		this.calendar = calendar;
	}

	public ColorAxis getColorAxis()
	{
		return this.colorAxis;
	}

	public void setColorAxis(final ColorAxis colorAxis)
	{
		this.colorAxis = colorAxis;
	}

	public Boolean getForceIFrame()
	{
		return this.forceIFrame;
	}

	public void setForceIFrame(final Boolean forceIFrame)
	{
		this.forceIFrame = forceIFrame;
	}

	public NoDataPattern getNoDataPattern()
	{
		return this.noDataPattern;
	}

	public void setNoDataPattern(final NoDataPattern noDataPattern)
	{
		this.noDataPattern = noDataPattern;
	}

	@Override
	protected void createConfiguration(final ObjectHelper obj)
	{
		super.createConfiguration(obj);

		obj.putIfNotNull("calendar", this.calendar);
		obj.putIfNotNull("colorAxis", this.colorAxis);
		obj.putIfNotNull("forceIFrame", this.forceIFrame);
		obj.putIfNotNull("noDataPattern", this.noDataPattern);
	}
}

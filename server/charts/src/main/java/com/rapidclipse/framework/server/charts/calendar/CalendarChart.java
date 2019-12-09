/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.calendar;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColorAxis;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("calendar-chart")
public class CalendarChart extends AbstractChart
	implements AllowsIFrame, HasColorAxis, HasChartSize
{
	public CalendarChart()
	{
		super("Calendar", "calendar");
	}

	public ChartModel
		initDefaultColumns(final String dateColumn, final Column.Type dateColumnType, final String valueColumn)
	{
		validateColumnType(dateColumnType, "date column", Column.Type.DATE, Column.Type.DATE_TIME,
			Column.Type.TIME_OF_DAY);

		return getModel().removeAll()
			.addColumn(Column.New(dateColumnType, dateColumn))
			.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
	}

	public Calendar getCalendar()
	{
		return properties().get("calendar");
	}

	public void setCalendar(final Calendar calendar)
	{
		properties().put("calendar", calendar);
	}

	public NoDataPattern getNoDataPattern()
	{
		return properties().get("noDataPattern");
	}

	public void setNoDataPattern(final NoDataPattern noDataPattern)
	{
		properties().put("noDataPattern", noDataPattern);
	}
}

/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.calendar;

import java.time.LocalDate;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Column.Type;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColorAxis;
import com.rapidclipse.framework.server.charts.HasTitle;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("calendar-chart")
public class CalendarChart extends AbstractChart
	implements AllowsIFrame, HasColorAxis, HasChartSize, HasTitle
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
		return properties().get("calendar", null);
	}

	public void setCalendar(final Calendar calendar)
	{
		properties().put("calendar", calendar);
	}

	public NoDataPattern getNoDataPattern()
	{
		return properties().get("noDataPattern", null);
	}

	public void setNoDataPattern(final NoDataPattern noDataPattern)
	{
		properties().put("noDataPattern", noDataPattern);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumns("Date", Type.DATE, "Students")
			.addRow(LocalDate.of(2012, 3, 13), 50)
			.addRow(LocalDate.of(2012, 3, 14), 50)
			.addRow(LocalDate.of(2012, 3, 15), 49)
			.addRow(LocalDate.of(2012, 3, 16), 48)
			.addRow(LocalDate.of(2012, 3, 17), 50)
			.addRow(LocalDate.of(2012, 4, 1), 50)
			.addRow(LocalDate.of(2012, 4, 2), 50)
			.addRow(LocalDate.of(2012, 4, 3), 49)
			.addRow(LocalDate.of(2012, 4, 4), 48)
			.addRow(LocalDate.of(2012, 4, 5), 50)
			.addRow(LocalDate.of(2012, 5, 4), 40)
			.addRow(LocalDate.of(2012, 5, 5), 50)
			.addRow(LocalDate.of(2012, 5, 10), 48)
			.addRow(LocalDate.of(2012, 5, 11), 50)
			.addRow(LocalDate.of(2012, 5, 12), 42)
			.addRow(LocalDate.of(2012, 5, 20), 45)
			.addRow(LocalDate.of(2012, 5, 21), 46)
			.addRow(LocalDate.of(2012, 5, 29), 45)
			.addRow(LocalDate.of(2013, 3, 13), 40)
			.addRow(LocalDate.of(2013, 3, 14), 40)
			.addRow(LocalDate.of(2013, 3, 15), 39)
			.addRow(LocalDate.of(2013, 3, 16), 38)
			.addRow(LocalDate.of(2013, 3, 17), 40)
			.addRow(LocalDate.of(2013, 4, 1), 40)
			.addRow(LocalDate.of(2013, 4, 2), 40)
			.addRow(LocalDate.of(2013, 4, 3), 49)
			.addRow(LocalDate.of(2013, 4, 4), 48)
			.addRow(LocalDate.of(2013, 4, 5), 40)
			.addRow(LocalDate.of(2013, 5, 4), 40)
			.addRow(LocalDate.of(2013, 5, 5), 50)
			.addRow(LocalDate.of(2013, 5, 12), 48)
			.addRow(LocalDate.of(2013, 5, 13), 40)
			.addRow(LocalDate.of(2013, 5, 19), 32)
			.addRow(LocalDate.of(2013, 5, 23), 45)
			.addRow(LocalDate.of(2013, 5, 24), 36)
			.addRow(LocalDate.of(2013, 5, 30), 45);

		setTitle("Attendence");
	}
}

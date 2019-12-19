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

package com.rapidclipse.framework.server.charts.gantt;

import java.time.Duration;
import java.time.LocalDate;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("gantt-chart")
public class GanttChart extends AbstractChart
	implements HasBackground, HasChartSize
{
	public GanttChart()
	{
		super("Gantt", "gantt");
	}
	
	public ChartModel initDefaultColumns()
	{
		return initDefaultColumns("Task ID", "Task name", null, "Start Date", "End Date", "Duration",
			"Percent Complete", "Dependencies");
	}
	
	public ChartModel initDefaultColumnsWithResourceId()
	{
		return initDefaultColumns("Task ID", "Task name", "Resource ID", "Start Date", "End Date", "Duration",
			"Percent Complete", "Dependencies");
	}
	
	public ChartModel
		initDefaultColumns(
			final String taskIdColumn,
			final String taskNameColumn,
			final String resourceIdColumn,
			final String startDateColumn,
			final String endDateColumn,
			final String durationColumn,
			final String percentCompleteColumn,
			final String dependenciesColumn)
	{
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, taskIdColumn))
			.addColumn(Column.New(Column.Type.STRING, taskNameColumn));
		if(resourceIdColumn != null)
		{
			model.addColumn(Column.New(Column.Type.STRING, resourceIdColumn));
		}
		return model.addColumn(Column.New(Column.Type.DATE, startDateColumn))
			.addColumn(Column.New(Column.Type.DATE, endDateColumn))
			.addColumn(Column.New(Column.Type.NUMBER, durationColumn))
			.addColumn(Column.New(Column.Type.NUMBER, percentCompleteColumn))
			.addColumn(Column.New(Column.Type.STRING, dependenciesColumn));
	}
	
	public Gantt getGantt()
	{
		return properties().get("gantt", null);
	}
	
	public void setGantt(final Gantt gantt)
	{
		properties().put("gantt", gantt);
	}
	
	@Override
	public void showSampleData()
	{
		initDefaultColumns()
			.addRow("Research", "Find sources",
				LocalDate.of(2015, 1, 1), LocalDate.of(2015, 1, 5), null, 100, null)
			.addRow("Write", "Write paper",
				null, LocalDate.of(2015, 1, 9), Duration.ofDays(3).toMillis(), 25, "Research,Outline")
			.addRow("Cite", "Create bibliography",
				null, LocalDate.of(2015, 1, 7), Duration.ofDays(1).toMillis(), 20, "Research")
			.addRow("Complete", "Hand in paper",
				null, LocalDate.of(2015, 1, 10), Duration.ofDays(1).toMillis(), 0, "Cite,Write")
			.addRow("Outline", "Outline paper",
				null, LocalDate.of(2015, 1, 6), Duration.ofDays(1).toMillis(), 100, "Research");
	}
}

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

package com.rapidclipse.framework.server.charts.gantt;

import com.rapidclipse.framework.server.charts.ChartBase;
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
public class GanttChart extends ChartBase
	implements HasBackground, HasChartSize
{
	public GanttChart()
	{
		super("Gantt", "gantt");
	}

	public ChartModel initDefaultColumns()
	{
		return initDefaultColumns("task ID", "task name", null, "start date", "end date", "duration",
			"percent complete", "dependencies");
	}

	public ChartModel initDefaultColumnsWithResourceId()
	{
		return initDefaultColumns("task ID", "task name", "resource ID", "start date", "end date", "duration",
			"percent complete", "dependencies");
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
		return properties().get("gantt");
	}

	public void setGantt(final Gantt gantt)
	{
		properties().put("gantt", gantt);
	}
}

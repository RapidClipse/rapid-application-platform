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
package com.rapidclipse.framework.server.charts.sankey;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("sankey-chart")
public class SankeyChart extends AbstractChart
	implements AllowsIFrame, HasChartSize, HasTooltip
{
	public SankeyChart()
	{
		super("Sankey", "sankey");
	}

	public ChartModel initDefaultColumns()
	{
		return initDefaultColumns("source", "destination", "value");
	}

	public ChartModel
		initDefaultColumns(final String sourceColumn, final String destinationColumn, final String valueColumn)
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, sourceColumn))
			.addColumn(Column.New(Column.Type.STRING, destinationColumn))
			.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
	}
	
	public Sankey getSankey()
	{
		return properties().get("sankey");
	}
	
	public void setSankey(final Sankey sankey)
	{
		properties().put("sankey", sankey);
	}
}

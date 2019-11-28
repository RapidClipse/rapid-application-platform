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

package com.rapidclipse.framework.server.charts.sankey;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
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
public class SankeyChart extends ChartBase
	implements AllowsIFrame, HasChartSize, HasTooltip
{
	public SankeyChart()
	{
		super("Sankey", "sankey");
	}
	
	public ChartModel initDefaultColumns()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "source"))
			.addColumn(Column.New(Column.Type.STRING, "destination"))
			.addColumn(Column.New(Column.Type.NUMBER, "value"));
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

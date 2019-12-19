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

import java.util.Arrays;
import java.util.List;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
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
		return initDefaultColumns("Source", "Destination", "Value");
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
		return properties().get("sankey", null);
	}

	public void setSankey(final Sankey sankey)
	{
		properties().put("sankey", sankey);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumns("From", "To", "Weight")
			.addRow("Brazil", "Portugal", 5)
			.addRow("Brazil", "France", 1)
			.addRow("Brazil", "Spain", 1)
			.addRow("Brazil", "England", 1)
			.addRow("Canada", "Portugal", 1)
			.addRow("Canada", "France", 5)
			.addRow("Canada", "England", 1)
			.addRow("Mexico", "Portugal", 1)
			.addRow("Mexico", "France", 1)
			.addRow("Mexico", "Spain", 5)
			.addRow("Mexico", "England", 1)
			.addRow("USA", "Portugal", 1)
			.addRow("USA", "France", 1)
			.addRow("USA", "Spain", 1)
			.addRow("USA", "England", 5)
			.addRow("Portugal", "Angola", 2)
			.addRow("Portugal", "Senegal", 1)
			.addRow("Portugal", "Morocco", 1)
			.addRow("Portugal", "South Africa", 3)
			.addRow("France", "Angola", 1)
			.addRow("France", "Senegal", 3)
			.addRow("France", "Mali", 3)
			.addRow("France", "Morocco", 3)
			.addRow("France", "South Africa", 1)
			.addRow("Spain", "Senegal", 1)
			.addRow("Spain", "Morocco", 3)
			.addRow("Spain", "South Africa", 1)
			.addRow("England", "Angola", 1)
			.addRow("England", "Senegal", 1)
			.addRow("England", "Morocco", 2)
			.addRow("England", "South Africa", 7)
			.addRow("South Africa", "China", 5)
			.addRow("South Africa", "India", 1)
			.addRow("South Africa", "Japan", 3)
			.addRow("Angola", "China", 5)
			.addRow("Angola", "India", 1)
			.addRow("Angola", "Japan", 3)
			.addRow("Senegal", "China", 5)
			.addRow("Senegal", "India", 1)
			.addRow("Senegal", "Japan", 3)
			.addRow("Mali", "China", 5)
			.addRow("Mali", "India", 1)
			.addRow("Mali", "Japan", 3)
			.addRow("Morocco", "China", 5)
			.addRow("Morocco", "India", 1)
			.addRow("Morocco", "Japan", 3);

		final List<String> colors =
			Arrays.asList("#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
				"#cab2d6", "#ffff99", "#1f78b4", "#33a02c");
		setSankey(Sankey.New(
			Link.New(null, colors, ColorMode.GRADIENT),
			Node.Builder().colors(colors).build()));
	}
}

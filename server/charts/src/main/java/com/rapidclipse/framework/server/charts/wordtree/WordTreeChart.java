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
package com.rapidclipse.framework.server.charts.wordtree;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasFontName;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("wordtree-chart")
public class WordTreeChart extends AbstractChart
	implements HasColors, AllowsIFrame, HasFontName, HasChartSize
{
	public WordTreeChart()
	{
		super("WordTree", "wordtree");
	}

	public ChartModel
		initDefaultColumnsImplicit(final String textColumn, final String sizeColumn, final String styleColumn)
	{
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, textColumn));
		if(sizeColumn != null)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, sizeColumn));
		}
		if(styleColumn != null)
		{
			model.addColumn(Column.New(Column.Type.STRING, styleColumn));
		}
		return model;
	}

	public ChartModel initDefaultColumnsExplicit(
		final String idColumn,
		final String textColumn,
		final String parentColumn,
		final String sizeColumn,
		final String styleColumn)
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, idColumn))
			.addColumn(Column.New(Column.Type.STRING, textColumn))
			.addColumn(Column.New(Column.Type.NUMBER, parentColumn))
			.addColumn(Column.New(Column.Type.NUMBER, sizeColumn))
			.addColumn(Column.New(Column.Type.STRING, styleColumn));
	}
	
	public Number getMaxFontSize()
	{
		return properties().get("maxFontSize");
	}
	
	public void setMaxFontSize(final Number maxFontSize)
	{
		properties().put("maxFontSize", maxFontSize);
	}
	
	public WordTree getWordTree()
	{
		return properties().get("wordTree");
	}
	
	public void setWordTree(final WordTree wordTree)
	{
		properties().put("wordTree", wordTree);
	}
}

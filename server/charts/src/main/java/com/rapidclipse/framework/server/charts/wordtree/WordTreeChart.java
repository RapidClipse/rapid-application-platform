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

package com.rapidclipse.framework.server.charts.wordtree;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
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
public class WordTreeChart extends ChartBase
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

	public Double getMaxFontSize()
	{
		return properties().get("maxFontSize");
	}

	public void setMaxFontSize(final Double maxFontSize)
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

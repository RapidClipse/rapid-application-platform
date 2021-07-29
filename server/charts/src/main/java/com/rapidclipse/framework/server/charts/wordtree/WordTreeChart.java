/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.wordtree;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Column.Role;
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

	public ChartModel initDefaultColumnsImplicit(final String textColumn)
	{
		return initDefaultColumnsImplicit(textColumn, null, null);
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
			model.addColumn(Column.New(Column.Type.STRING, styleColumn, Role.STYLE));
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
			.addColumn(Column.New(Column.Type.NUMBER, idColumn))
			.addColumn(Column.New(Column.Type.STRING, textColumn))
			.addColumn(Column.New(Column.Type.NUMBER, parentColumn))
			.addColumn(Column.New(Column.Type.NUMBER, sizeColumn))
			.addColumn(Column.New(Column.Type.STRING, styleColumn, Role.STYLE));
	}

	public Number getMaxFontSize()
	{
		return properties().get("maxFontSize", null);
	}

	public void setMaxFontSize(final Number maxFontSize)
	{
		properties().put("maxFontSize", maxFontSize);
	}

	public WordTree getWordTree()
	{
		return properties().get("wordTree", null);
	}

	public void setWordTree(final WordTree wordTree)
	{
		properties().put("wordTree", wordTree);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumnsExplicit("id", "childLabel", "parent", "size", "style")
			.addRow(0, "Life", -1, 1, "black")
			.addRow(1, "Archaea", 0, 1, "black")
			.addRow(2, "Eukarya", 0, 5, "black")
			.addRow(3, "Bacteria", 0, 1, "black")
			.addRow(4, "Crenarchaeota", 1, 1, "black")
			.addRow(5, "Euryarchaeota", 1, 1, "black")
			.addRow(6, "Korarchaeota", 1, 1, "black")
			.addRow(7, "Nanoarchaeota", 1, 1, "black")
			.addRow(8, "Thaumarchaeota", 1, 1, "black")
			.addRow(9, "Amoebae", 2, 1, "black")
			.addRow(10, "Plants", 2, 1, "black")
			.addRow(11, "Chromalveolata", 2, 1, "black")
			.addRow(12, "Opisthokonta", 2, 5, "black")
			.addRow(13, "Rhizaria", 2, 1, "black")
			.addRow(14, "Excavata", 2, 1, "black")
			.addRow(15, "Animalia", 12, 5, "black")
			.addRow(16, "Fungi", 12, 2, "black")
			.addRow(17, "Parazoa", 15, 2, "black")
			.addRow(18, "Eumetazoa", 15, 5, "black")
			.addRow(19, "Radiata", 18, 2, "black")
			.addRow(20, "Bilateria", 18, 5, "black")
			.addRow(21, "Orthonectida", 20, 2, "black")
			.addRow(22, "Rhombozoa", 20, 2, "black")
			.addRow(23, "Acoelomorpha", 20, 1, "black")
			.addRow(24, "Deuterostomia", 20, 5, "black")
			.addRow(25, "Chaetognatha", 20, 2, "black")
			.addRow(26, "Protostomia", 20, 2, "black")
			.addRow(27, "Chordata", 24, 5, "black")
			.addRow(28, "Hemichordata", 24, 1, "black")
			.addRow(29, "Echinodermata", 24, 1, "black")
			.addRow(30, "Xenoturbellida", 24, 1, "black")
			.addRow(31, "Vetulicolia", 24, 1, "black");

		setWordTree(WordTree.Builder().format(Format.EXPLICIT).type(Type.SUFFIX).build());
	}
}

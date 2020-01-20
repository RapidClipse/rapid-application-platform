/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.charts.histogram;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.CanInterpolateNulls;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasBar;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasDataOpacity;
import com.rapidclipse.framework.server.charts.HasFocusTarget;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasOrientation;
import com.rapidclipse.framework.server.charts.HasSeries;
import com.rapidclipse.framework.server.charts.HasStackMode;
import com.rapidclipse.framework.server.charts.HasTheme;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.rapidclipse.framework.server.charts.HasVAxes;
import com.rapidclipse.framework.server.charts.Legend;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("histogram-chart")
public class HistogramChart extends AbstractChart
	implements HasAnimation, HasAxisTitlesPosition, HasBackground, HasBar, HasChartArea, HasColors, HasDataOpacity,
	HasInteractivity, HasFocusTarget, HasFont, AllowsIFrame, HasHAxis, HasChartSize, CanInterpolateNulls, HasStackMode,
	HasLegend, HasOrientation, HasCategories, HasTheme, HasTitlePosition, HasTooltip, HasVAxes,
	HasSeries<HistogramSeries>
{
	public HistogramChart()
	{
		super("Histogram");
	}

	public ChartModel initDefaultColumnsSingleSeries(final String labelColumn, final String valueColumn)
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, labelColumn))
			.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
	}

	public ChartModel initDefaultColumnsMultipleSeries(final String... seriesColumns)
	{
		final ChartModel model = getModel().removeAll();
		for(final String seriesColumn : seriesColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, seriesColumn));
		}
		return model;
	}

	public Histogram getHistogram()
	{
		return properties().get("histogram", null);
	}

	public void setHistogram(final Histogram histogram)
	{
		properties().put("histogram", histogram);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumnsSingleSeries("Dinosaur", "Length")
			.addRow("Acrocanthosaurus (top-spined lizard)", 12.2)
			.addRow("Albertosaurus (Alberta lizard)", 9.1)
			.addRow("Allosaurus (other lizard)", 12.2)
			.addRow("Apatosaurus (deceptive lizard)", 22.9)
			.addRow("Archaeopteryx (ancient wing)", 0.9)
			.addRow("Argentinosaurus (Argentina lizard)", 36.6)
			.addRow("Baryonyx (heavy claws)", 9.1)
			.addRow("Brachiosaurus (arm lizard)", 30.5)
			.addRow("Ceratosaurus (horned lizard)", 6.1)
			.addRow("Coelophysis (hollow form)", 2.7)
			.addRow("Compsognathus (elegant jaw)", 0.9)
			.addRow("Deinonychus (terrible claw)", 2.7)
			.addRow("Diplodocus (double beam)", 27.1)
			.addRow("Dromicelomimus (emu mimic)", 3.4)
			.addRow("Gallimimus (fowl mimic)", 5.5)
			.addRow("Mamenchisaurus (Mamenchi lizard)", 21.0)
			.addRow("Megalosaurus (big lizard)", 7.9)
			.addRow("Microvenator (small hunter)", 1.2)
			.addRow("Ornithomimus (bird mimic)", 4.6)
			.addRow("Oviraptor (egg robber)", 1.5)
			.addRow("Plateosaurus (flat lizard)", 7.9)
			.addRow("Sauronithoides (narrow-clawed lizard)", 2.0)
			.addRow("Seismosaurus (tremor lizard)", 45.7)
			.addRow("Spinosaurus (spiny lizard)", 12.2)
			.addRow("Supersaurus (super lizard)", 30.5)
			.addRow("Tyrannosaurus (tyrant lizard)", 15.2)
			.addRow("Ultrasaurus (ultra lizard)", 30.5)
			.addRow("Velociraptor (swift robber)", 1.8);

		setTitle("Lengths of dinosaurs, in meters");
		setLegend(Legend.None());
	}
}

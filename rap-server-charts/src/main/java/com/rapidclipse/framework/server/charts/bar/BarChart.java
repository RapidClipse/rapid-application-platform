/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.bar;

import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.HasHAxes;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("bar-chart")
public class BarChart extends AbstractBarChart
	implements HasHAxes
{
	public BarChart()
	{
		super("BarChart");
	}

	public BarChart(final ChartModel before, final ChartModel after)
	{
		this();

		setModel(before, after);
	}
}

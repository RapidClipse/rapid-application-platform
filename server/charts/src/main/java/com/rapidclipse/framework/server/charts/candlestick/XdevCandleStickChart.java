/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.charts.candlestick;

import com.rapidclipse.framework.server.charts.ChartJsBuilder;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.config.IdGenerator;
import com.vaadin.flow.component.HtmlContainer;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
@Tag("CandleStick-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevCandleStickChart extends HtmlContainer
{
	private final CandleStickChartComponentState candleState = new CandleStickChartComponentState();
	private final String                         id;

	public XdevCandleStickChart()
	{
		super();
		this.id = IdGenerator.generateId();
		this.candleState.setConfig(new XdevCandleStickChartConfig());
	}

	/**
	 * Setting options for the chart.
	 *
	 * @param config
	 */
	public void setConfig(final XdevCandleStickChartConfig config)
	{
		this.candleState.setConfig(config);
	}

	/**
	 * Setting a model for the chart and will draw it new.
	 *
	 * @param model
	 */
	public void setModel(final XdevChartModel model)
	{
		this.candleState.setDataTable(model.getDataTable());
		this.setId(this.id);
		this.buildChart();
	}

	/**
	 * Draws the chart.
	 * setModel or buildChart should be the last methods to call.
	 */
	public void buildChart()
	{
		final ChartJsBuilder js   = new ChartJsBuilder(this.candleState.getDataTable(),
			this.candleState.getConfig().getOptions(), this.id, "CandlestickChart");
		final Page           page = UI.getCurrent().getPage();
		page.executeJs(js.constructChart());
	}

}

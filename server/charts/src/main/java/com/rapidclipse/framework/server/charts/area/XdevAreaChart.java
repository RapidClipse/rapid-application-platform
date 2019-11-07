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

package com.rapidclipse.framework.server.charts.area;

import java.util.Optional;

import com.rapidclipse.framework.server.charts.Row;
import com.rapidclipse.framework.server.charts.XdevChartModel;
import com.rapidclipse.framework.server.charts.config.IdGenerator;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.page.Page;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
@Tag("Area-chart")
@JavaScript("https://www.gstatic.com/charts/loader.js")
public class XdevAreaChart extends Component
{
	private final AreaChartComponentState areaState = new AreaChartComponentState();
	private final XdevAreaChartJsBuilder  jsBuilder;
	private final String                  id;

	public XdevAreaChart()
	{
		super();
		this.id = IdGenerator.generateId();

		this.areaState.setConfig(new XdevAreaChartConfig());
		this.jsBuilder = new XdevAreaChartJsBuilder(this.areaState, this.id);
	}
	
	public void setConfig(final XdevAreaChartConfig config)
	{
		this.areaState.setConfig(config);
	}
	
	public void setModel(final XdevChartModel model)
	{
		Row.createFromHashmap(model.getData()).forEach(row -> model.getDataTable().getRows().add(row));

		this.areaState.setDataTable(model.getDataTable());

		final Optional<Component> parent = this.getParent();
		if(parent.isPresent())
		{
			parent.get().setId(this.id);
		}
		else
		{
			this.setId(this.id);
		}
		final Page page = UI.getCurrent().getPage();
		page.executeJs(this.jsBuilder.constructChart());
	}
	
}

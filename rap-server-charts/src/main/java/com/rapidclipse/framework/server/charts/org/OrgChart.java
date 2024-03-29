/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.org;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsHtml;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("org-chart")
public class OrgChart extends AbstractChart
	implements AllowsHtml
{
	public OrgChart()
	{
		super("OrgChart", "orgchart");
	}

	public ChartModel initDefaultColumnsSimple()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "id"));
	}

	public ChartModel initDefaultColumnsWithParent()
	{
		return initDefaultColumnsSimple()
			.addColumn(Column.New(Column.Type.STRING, "parent"));
	}

	public ChartModel initDefaultColumnsWithParentAndTooltip()
	{
		return initDefaultColumnsWithParent()
			.addColumn(Column.New(Column.Type.STRING, "tooltip"));
	}

	public boolean getAllowCollapse()
	{
		return properties().get("allowCollapse", false);
	}

	public void setAllowCollapse(final boolean allowCollapse)
	{
		properties().put("allowCollapse", allowCollapse);
	}

	public String getNodeClass()
	{
		return properties().get("nodeClass", null);
	}

	public void setNodeClass(final String nodeClass)
	{
		properties().put("nodeClass", nodeClass);
	}

	public String getSelectedNodeClass()
	{
		return properties().get("selectedNodeClass", null);
	}

	public void setSelectedNodeClass(final String selectedNodeClass)
	{
		properties().put("selectedNodeClass", selectedNodeClass);
	}

	public Size getSize()
	{
		return properties().get("size", Size.MEDIUM);
	}

	public void setSize(final Size size)
	{
		properties().put("size", size);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumnsWithParentAndTooltip()
			.addRow("Mike", "", "The President")
			.addRow("Jim", "Mike", "VP")
			.addRow("Alice", "Mike", "")
			.addRow("Bob", "Jim", "Bob Sponge")
			.addRow("Carol", "Bob", "");
	}
}

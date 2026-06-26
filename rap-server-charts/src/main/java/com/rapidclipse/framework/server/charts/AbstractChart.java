/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import static java.util.Objects.requireNonNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.charts.JavaScriptable.ArrayHelper;
import com.rapidclipse.framework.server.charts.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.ComponentUtil;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.shared.Registration;

import elemental.json.JsonArray;
import elemental.json.JsonNumber;
import elemental.json.JsonObject;
import elemental.json.JsonValue;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
@JavaScript("https://www.gstatic.com/charts/loader.js")
public abstract class AbstractChart extends Composite<Div> implements Chart
{
	private final String     type;
	private final String[]   packages;
	private final Properties properties = new Properties();
	private ChartModel       model      = ChartModel.New();
	private ChartModel       modelBefore, modelAfter;
	private Selection        selection  = Selection.Empty();

	protected AbstractChart(final String type, final String... packages)
	{
		super();

		this.type     = type;
		this.packages = packages != null && packages.length > 0
			? packages
			: new String[]{"corechart"};

		addAttachListener(event -> refresh());
	}

	@Override
	public Properties properties()
	{
		return this.properties;
	}

	public void setModel(final ChartModel model)
	{
		this.model       = model;
		this.modelBefore = null;
		this.modelAfter  = null;
	}

	protected void setModel(final ChartModel before, final ChartModel after)
	{
		this.model       = null;
		this.modelBefore = before;
		this.modelAfter  = after;
	}
	
	public abstract void showSampleData();

	@Override
	public ChartModel getModel()
	{
		return this.model;
	}

	public void refresh()
	{
		final String js = createChartJs();
		UI.getCurrent().getPage().executeJs(js);
	}

	private String createChartJs()
	{
		final ObjectHelper loadOptions = new ObjectHelper();
		createLoadOptions(loadOptions);

		final StringBuilder sb = new StringBuilder();
		sb.append("google.charts.load('visualization', 'current', ").append(loadOptions.js()).append(");\n");
		sb.append("google.charts.setOnLoadCallback(drawChart);\n");
		sb.append("function drawChart(){\n");
		sb.append("var elem = document.getElementById('").append(this.id()).append("');\n");
		sb.append("var chart = new google.visualization.").append(this.type).append("(elem);\n");
		sb.append("elem.chart = chart;\n");
		sb.append("var configuration = ").append(this.properties.js()).append(";\n");
		if(this.modelBefore != null && this.modelAfter != null)
		{
			sb.append(this.modelBefore.js("before"));
			sb.append(this.modelAfter.js("after"));
			sb.append("var view = chart.computeDiff(before, after);\n");
		}
		else
		{
			sb.append(this.model.js("data"));
			sb.append("var view = new google.visualization.DataView(data);\n");
		}
		sb.append("chart.draw(view, configuration);\n");
		sb.append("google.visualization.events.addListener(chart, 'select', function() {\n");
		sb.append(" elem.$server.selectionChanged(chart.getSelection());");
		sb.append("});\n");
		sb.append("var resizeHandler = function(){chart.draw(view, configuration);}\n");
		sb.append("window.addEventListener('resize', function(){");
		sb.append(" if(this.resizeTimeout) clearTimeout(this.resizeTimeout);\n");
		sb.append(" this.resizeTimeout = setTimeout(resizeHandler,500);\n");
		sb.append("});\n");
		sb.append("}");

		return sb.toString();
	}

	private String id()
	{
		String id = getId().orElse(null);
		if(StringUtils.isEmpty(id))
		{
			setId(id = UUID.randomUUID().toString().replace("-", ""));
		}
		return id;
	}

	protected void createLoadOptions(final ObjectHelper obj)
	{
		obj.put("packages", new ArrayHelper().addAllStrings(Arrays.asList(this.packages)));
	}

	@SuppressWarnings({"rawtypes", "unchecked"})
	public <T extends AbstractChart> Registration
		addSelectionListener(final ComponentEventListener<SelectionEvent<T>> listener)
	{
		return ComponentUtil.addListener(this, SelectionEvent.class, (ComponentEventListener)listener);
	}

	private void fireSelectionChanged(final boolean fromClient)
	{
		ComponentUtil.fireEvent(this, new SelectionEvent<>(this, fromClient, this.selection));
	}

	public Selection getSelection()
	{
		return this.selection;
	}

	public void setSelection(final Selection selection)
	{
		if(!this.selection.equals(requireNonNull(selection)))
		{
			this.selection = selection;

			final String js = new StringBuilder()
				.append("this.chart.setSelection(").append(selection.js()).append(");").toString();
			getElement().executeJs(js);

			fireSelectionChanged(false);
		}
	}

	public void clearSelection()
	{
		setSelection(Selection.Empty());
	}

	@ClientCallable
	void selectionChanged(final JsonArray selectionArray)
	{
		final List<Selection.Item> items = new ArrayList<>();
		for(int i = 0, c = selectionArray.length(); i < c; i++)
		{
			final JsonObject obj    = selectionArray.get(i);
			final JsonValue  row    = obj.get("row");
			final JsonValue  column = obj.get("column");
			items.add(Selection.Item(
				row instanceof JsonNumber ? (int)row.asNumber() : null,
				column instanceof JsonNumber ? (int)column.asNumber() : null));
		}
		final Selection selection = Selection.New(items);
		if(!this.selection.equals(selection))
		{
			this.selection = selection;
			fireSelectionChanged(true);
		}
	}

	protected void
		validateColumnType(final Column.Type type, final String columnName, final Column.Type... allowedTypes)
	{
		if(!Arrays.asList(allowedTypes).contains(type))
		{
			throw new IllegalArgumentException(
				"Invalid column type for column '" + columnName + "': " + type.name() + ". Allowed types: " +
					Arrays.stream(allowedTypes).map(Enum::name).collect(Collectors.joining(", ")));
		}
	}
}

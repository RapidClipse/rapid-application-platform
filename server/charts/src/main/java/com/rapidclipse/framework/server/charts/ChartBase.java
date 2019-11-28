
package com.rapidclipse.framework.server.charts;

import static java.util.Objects.requireNonNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.util.JavaScriptable.ArrayHelper;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.ComponentUtil;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.shared.Registration;

import elemental.json.JsonArray;
import elemental.json.JsonNumber;
import elemental.json.JsonObject;
import elemental.json.JsonValue;


/**
 * @author XDEV Software
 *
 */
public class ChartBase extends Composite<Div> implements Chart
{
	private final String     type;
	private final String[]   packages;
	private final Properties properties = new Properties();
	private ChartModel       model      = ChartModel.New();
	private Selection        selection  = Selection.Empty();

	protected ChartBase(final String type, final String... packages)
	{
		super();

		this.type     = type;
		this.packages = packages != null && packages.length > 0
			? packages
			: new String[]{"corechart"};
	}

	@Override
	public Properties properties()
	{
		return this.properties;
	}

	public void setModel(final ChartModel model)
	{
		this.model = model;
	}

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
		sb.append(this.model.js("data"));
		sb.append("var view = new google.visualization.DataView(data);\n");
		sb.append("chart.draw(view, configuration);\n");
		sb.append("google.visualization.events.addListener(chart, 'select', function() {\n");
		sb.append(" elem.$server.selectionChanged(chart.getSelection());");
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
	public <T extends ChartBase> Registration
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
}

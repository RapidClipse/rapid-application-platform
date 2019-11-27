
package com.rapidclipse.framework.server.charts;

import static java.util.Objects.requireNonNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.util.JavaScriptable;
import com.rapidclipse.framework.server.util.JavaScriptable.ArrayHelper;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.ComponentUtil;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.shared.Registration;

import elemental.json.JsonArray;
import elemental.json.JsonNumber;
import elemental.json.JsonObject;
import elemental.json.JsonValue;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@JavaScript("https://www.gstatic.com/charts/loader.js")
public abstract class Chart extends Composite<Div> implements HasSize
{
	private final String   type;
	private final String[] packages;

	private ChartModel model     = ChartModel.New();
	private Selection  selection = Selection.Empty();

	private String       title;
	private TextStyle    titleTextStyle;
	private String       fontName;
	private Integer      fontSize;
	private List<String> colors;
	private Tooltip      tooltip;

	protected Chart(final String type, final String... packages)
	{
		super();

		this.type     = type;
		this.packages = packages != null && packages.length > 0
			? packages
			: new String[]{"corechart"};
	}

	public void setModel(final ChartModel model)
	{
		this.model = model;
	}

	public ChartModel getModel()
	{
		return this.model;
	}

	public String getTitle()
	{
		return this.title;
	}

	public void setTitle(final String title)
	{
		this.title = title;
	}

	public TextStyle getTitleTextStyle()
	{
		return this.titleTextStyle;
	}

	public void setTitleTextStyle(final TextStyle titleTextStyle)
	{
		this.titleTextStyle = titleTextStyle;
	}

	public String getFontName()
	{
		return this.fontName;
	}

	public void setFontName(final String fontName)
	{
		this.fontName = fontName;
	}

	public Integer getFontSize()
	{
		return this.fontSize;
	}

	public void setFontSize(final Integer fontSize)
	{
		this.fontSize = fontSize;
	}

	public List<String> getColors()
	{
		return this.colors;
	}

	public void setColors(final List<String> colors)
	{
		this.colors = colors;
	}

	public Tooltip getTooltip()
	{
		return this.tooltip;
	}

	public void setTooltip(final Tooltip tooltip)
	{
		this.tooltip = tooltip;
	}

	public void refresh()
	{
		final String js = createChartJs();
		UI.getCurrent().getPage().executeJs(js);
	}

	protected String createChartJs()
	{
		final ObjectHelper loadOptions = new ObjectHelper();
		createLoadOptions(loadOptions);

		final ObjectHelper configuration = new ObjectHelper();
		createConfiguration(configuration);

		final StringBuilder sb = new StringBuilder();
		sb.append("google.charts.load('visualization', 'current', ").append(loadOptions.js()).append(");\n");
		sb.append("google.charts.setOnLoadCallback(drawChart);\n");
		sb.append("function drawChart(){\n");
		sb.append("var elem = document.getElementById('").append(this.id()).append("');\n");
		sb.append("var chart = new google.visualization.").append(this.type).append("(elem);\n");
		sb.append("elem.chart = chart;\n");
		sb.append("var configuration = ").append(configuration.js()).append(";\n");
		sb.append(this.model.js("data"));
		sb.append("var view = new google.visualization.DataView(data);\n");
		sb.append("chart.draw(view, configuration);\n");
		sb.append("google.visualization.events.addListener(chart, 'select', function() {\n");
		sb.append(" elem.$server.selectionChanged(chart.getSelection());");
		sb.append("});\n");
		sb.append("}");

		return sb.toString();
	}

	protected void createLoadOptions(final ObjectHelper obj)
	{
		obj.put("packages", new ArrayHelper().addAllStrings(Arrays.asList(this.packages)));
	}

	protected void createConfiguration(final ObjectHelper obj)
	{
		obj.putIfNotNull("title", this.title);
		obj.putIfNotNull("titleTextStyle", this.titleTextStyle);
		obj.putIfNotNull("fontName", this.fontName);
		obj.putIfNotNull("fontSize", this.fontSize);
		obj.putIfNotNull("colors", new ArrayHelper().addAllStrings(this.colors));
		obj.putIfNotNull("tooltip", this.tooltip);
	}

	protected <K, JS extends JavaScriptable> void
		putIfNotNull(final ObjectHelper obj, final String key, final Map<K, JS> values)
	{
		if(values != null && values.size() > 0)
		{
			final ObjectHelper valuesObj = new ObjectHelper();
			values.entrySet().forEach(e -> valuesObj.putIfNotNull(e.getKey().toString(), e.getValue()));
			obj.put(key, valuesObj);
		}
	}

	protected String id()
	{
		String id = getId().orElse(null);
		if(StringUtils.isEmpty(id))
		{
			setId(id = UUID.randomUUID().toString().replace("-", ""));
		}
		return id;
	}

	@SuppressWarnings({"rawtypes", "unchecked"})
	public <T extends Chart> Registration addSelectionListener(final ComponentEventListener<SelectionEvent<T>> listener)
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

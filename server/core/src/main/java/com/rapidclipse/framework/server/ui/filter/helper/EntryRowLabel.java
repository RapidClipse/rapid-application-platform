
package com.rapidclipse.framework.server.ui.filter.helper;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.rapidclipse.framework.server.ui.filter.FilterValueEditorComposite;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;


/**
 * @author XDEV Software
 *
 */
public class EntryRowLabel
{
	HorizontalLayout  layout = new HorizontalLayout();
	FilterEntryEditor editor;

	@SuppressWarnings("rawtypes")
	private List<FilterValueEditorComposite> values;
	private String                           property;
	private String                           operator;
	private final StringBuilder              description = new StringBuilder();
	private static final DateTimeFormatter   FORMATTER   = DateTimeFormatter.ofPattern("YYYY.MM.dd");

	public EntryRowLabel(final FilterEntryEditor editor)
	{
		this.editor = editor;
		setVariables();
		this.layout = createEntry();
		
	}

	public EntryRowLabel()
	{
		this.editor = null;
	}

	/**
	 * @param shortLayout
	 * @param longLayout
	 * @param editor
	 */
	public EntryRowLabel(
		final HorizontalLayout layout,
		final FilterEntryEditor editor)
	{
		super();
		this.editor = editor;
		setVariables();
		this.layout = layout;
		
	}
	
	/**
	 * @return the editor
	 */
	public FilterEntryEditor getEditor()
	{
		return this.editor;
	}

	/**
	 * @param editor
	 *            the editor to set
	 */
	public void setEditor(final FilterEntryEditor editor)
	{
		this.editor = editor;
	}

	/**
	 * @return the layout
	 */
	public HorizontalLayout getLayout()
	{
		return this.layout;
	}

	/**
	 * @param layout
	 *            the layout to set
	 */
	public void setLayout(final HorizontalLayout layout)
	{
		this.layout = layout;
	}

	private void setVariables()
	{
		this.values   = this.editor.getValueEditors();
		this.operator = this.editor.getSelectedOperator().name();
		this.property = this.editor.getSelectedProperty().caption();
	}

	/**
	 * Creates the EntryRow which can be shown in a {@link Div} or somewhere else
	 *
	 * @return -> {@link HorizontalLayout}
	 */
	private HorizontalLayout createEntry()
	{
		final HorizontalLayout row = new HorizontalLayout();
		row.addClassName(StringResourceUtils.getResourceString("entryRowLabel", this));
		row.setEnabled(true);

		this.description.append(this.property + " -> " + this.operator);
		buildRow(row);

		row.getElement().setProperty("title", this.description.toString());
		return row;
	}
	
	/**
	 * Build the {@link Label} which is added to the <b>row</b>, to show the selected Filter
	 * 
	 * @param row
	 *            -> {@link HorizontalLayout}
	 */
	private void buildRow(
		final HorizontalLayout row)
	{
		final Label label = new Label();
		label.addClassName("label");
		label.add(this.property + " " + this.operator);
		if(this.values != null)
		{
			for(final FilterValueEditorComposite<?, ?> value : this.values)
			{
				if(value.component() instanceof DatePicker)
				{
					final DatePicker datepicker = (DatePicker)value.component();
					
					final LocalDate date = datepicker.getValue();
					
					label.add(" " + FORMATTER.format(date));
					this.description.append(" -> " + FORMATTER.format(date));
				}
				else
				{
					label.add(" " + value.getValue().toString());
					this.description.append(" -> " + value.getValue().toString());
				}
			}
			row.add(label);
		}
	}
	
}

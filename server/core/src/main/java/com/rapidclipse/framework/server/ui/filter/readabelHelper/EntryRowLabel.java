
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import java.util.List;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.rapidclipse.framework.server.ui.filter.FilterValueEditorComposite;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;


/**
 * @author XDEV Software
 *
 */
public class EntryRowLabel
{
	HorizontalLayout  shortLayout = new HorizontalLayout();
	HorizontalLayout  longLayout  = new HorizontalLayout();
	FilterEntryEditor editor;

	public EntryRowLabel(final FilterEntryEditor editor)
	{
		this.editor      = editor;
		this.longLayout  = createEntry(false);
		this.shortLayout = createEntry(true);
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
		final HorizontalLayout shortLayout,
		final HorizontalLayout longLayout,
		final FilterEntryEditor editor)
	{
		super();
		this.shortLayout = shortLayout;
		this.longLayout  = longLayout;
		this.editor      = editor;
	}

	private String shortenString(final String s, final int length, final boolean dots)
	{
		String string = "";

		if(s.length() >= length)
		{
			if(dots)
			{
				string = s.substring(0, length) + "...";
			}
			else
			{
				string = s.substring(0, length);
			}
		}
		else
		{
			return s;
		}

		return string;
	}

	private Icon createArrow()
	{
		final Icon arrow = VaadinIcon.ARROW_RIGHT.create();
		arrow.setSize("20px");
		// The Vaadinblue if you .setIcon() to a Component
		arrow.setColor("#1676F3");
		return arrow;
	}

	/**
	 * @return the shortLayout
	 */
	public HorizontalLayout getShortLayout()
	{
		return this.shortLayout;
	}

	/**
	 * @param shortLayout
	 *            the shortLayout to set
	 */
	public void setShortLayout(final HorizontalLayout shortLayout)
	{
		this.shortLayout = shortLayout;
	}

	/**
	 * @return the longLayout
	 */
	public HorizontalLayout getLongLayout()
	{
		return this.longLayout;
	}

	/**
	 * @param longLayout
	 *            the longLayout to set
	 */
	public void setLongLayout(final HorizontalLayout longLayout)
	{
		this.longLayout = longLayout;
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
	 * Creates the EntryRow which can be shown in a {@link Div} or somewhere else
	 * 
	 * @param small
	 *            -> {@link Boolean} to set if the small or normal row should be created
	 * @return -> {@link HorizontalLayout}
	 */
	@SuppressWarnings("rawtypes")
	private HorizontalLayout createEntry(final boolean small)
	{
		final HorizontalLayout row = new HorizontalLayout();
		row.addClassName(StringResourceUtils.getResourceString("entryRowLabel", this));
		row.setEnabled(true);

		final List<FilterValueEditorComposite> values      = this.editor.getValueEditors();
		final String                           property    = this.editor.getSelectedProperty().caption();
		final String                           operator    = this.editor.getSelectedOperator().name();
		final StringBuilder                    description = new StringBuilder();
		description.append(property + " -> " + operator);
		if(small)
		{
			createShort(row, property, operator, values, description);
		}
		else
		{
			createLong(row, property, operator, values, description);
		}

		row.getElement().setProperty("title", description.toString());
		return row;
	}

	/**
	 * Creates the long/normal row
	 * 
	 * @param row
	 *            -> {@link HorizontalLayout}
	 * @param property
	 *            -> {@link String}
	 * @param operator
	 *            -> {@link String}
	 * @param values
	 *            -> {@link List} of type {@link FilterValueEditorComposite}
	 * @param description
	 *            -> {@link StringBuilder}
	 */
	@SuppressWarnings("rawtypes")
	private void createLong(
		final HorizontalLayout row,
		final String property,
		final String operator,
		final List<FilterValueEditorComposite> values,
		final StringBuilder description)
	{
		final int cutBy = 10;
		row.add(new Label(property));
		row.add(createArrow());
		row.add(new Label(operator));

		if(values != null)
		{
			for(final FilterValueEditorComposite<?, ?> value : values)
			{
				row.add(createArrow());

				if(value.component() instanceof DatePicker)
				{
					row.add(new Label(" " + shortenString(value.getValue().toString(), cutBy, false)));
					description.append(" -> " + shortenString(value.getValue().toString(), cutBy, false));
				}
				else
				{
					row.add(new Label(" " + shortenString(value.getValue().toString(), cutBy, true)));
					description.append(" -> " + value.getValue().toString());
				}
			}

			row.getElement().setProperty("title", description.toString());
		}
	}

	/**
	 * Creates the short row
	 * 
	 * @param row
	 *            -> {@link HorizontalLayout}
	 * @param property
	 *            -> {@link String}
	 * @param operator
	 *            -> {@link String}
	 * @param values
	 *            -> {@link List} of type {@link FilterValueEditorComposite}
	 * @param description
	 *            -> {@link StringBuilder}
	 */
	@SuppressWarnings("rawtypes")
	private void createShort(
		final HorizontalLayout row,
		final String property,
		final String operator,
		final List<FilterValueEditorComposite> values,
		final StringBuilder description)
	{
		int cutBy = 5;
		row.add(new Label(shortenString(property, cutBy, true)));
		row.add(createArrow());
		row.add(new Label(shortenString(operator, cutBy, true)));

		if(values != null)
		{
			for(final FilterValueEditorComposite<?, ?> value : values)
			{

				row.add(createArrow());

				if(values.size() > 1)
				{
					cutBy = 1;
				}
				row.add(new Label("" + shortenString(value.getValue().toString(), cutBy, true)));

				if(value.component() instanceof DatePicker)
				{
					description.append(" -> " + shortenString(value.getValue().toString(), 10, false));
				}
				else
				{
					description.append(" -> " + value.getValue().toString());
				}
			}
		}
	}
}

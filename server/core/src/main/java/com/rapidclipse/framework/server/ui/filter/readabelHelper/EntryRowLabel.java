
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import java.util.List;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.rapidclipse.framework.server.ui.filter.FilterValueEditorComposite;
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
		this.longLayout  = createLong();
		this.shortLayout = createShort();
	}

	/**
	 * @param editor2
	 * @return
	 */
	private HorizontalLayout createShort()
	{
		final int              cutBy = 5;
		final HorizontalLayout row   = new HorizontalLayout();
		row.addClassName(StringResourceUtils.getResourceString("entryRowLabel", this));
		row.setEnabled(true);
		
		final List<FilterValueEditorComposite> values = this.editor.getValueEditors();
		
		final String property = this.editor.getSelectedProperty().caption();
		row.add(new Label("" + shortenString(property, cutBy)));
		final String operator = this.editor.getSelectedOperator().name();
		row.add(new Label("\t" + shortenString(operator, cutBy)));
		final StringBuilder description = new StringBuilder();
		description.append(property + " -> " + operator);
		if(values != null)
		{
			
			for(final FilterValueEditorComposite<?, ?> value : values)
			{
				row.add(new Label("\t-> " + shortenString(value.getValue().toString(), cutBy)));
				description.append(" -> " + value.getValue().toString());
			}
		}
		
		row.getElement().setProperty("title", description.toString());
		return row;
	}

	/**
	 * @param editor2
	 * @return
	 */
	private HorizontalLayout createLong()
	{
		final int              cutBy = 5;
		final HorizontalLayout row   = new HorizontalLayout();
		row.addClassName(StringResourceUtils.getResourceString("entryRowLabel", this));
		row.setEnabled(true);
		
		final List<FilterValueEditorComposite> values = this.editor.getValueEditors();
		
		final String property = this.editor.getSelectedProperty().caption();
		row.add(new Label("" + property));
		final String operator = this.editor.getSelectedOperator().name();
		row.add(createArrow());
		row.add(new Label(" " + operator));

		final StringBuilder description = new StringBuilder();
		description.append(property + " -> " + operator);

		if(values != null)
		{
			
			for(final FilterValueEditorComposite<?, ?> value : values)
			{
				row.add(createArrow());
				row.add(new Label(" " + shortenString(value.getValue().toString(), cutBy)));
				description.append(" -> " + value.getValue().toString());
			}
		}
		
		// row.getElement().setProperty("title", description.toString());
		return row;
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

	private String shortenString(final String s, final int length)
	{
		String string = "";
		if(s.length() >= length)
		{
			string = s.substring(0, length) + "...";
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
		arrow.setColor("blue");
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
	
}

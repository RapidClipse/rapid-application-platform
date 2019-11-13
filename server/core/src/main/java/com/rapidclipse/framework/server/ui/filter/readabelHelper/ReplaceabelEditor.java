
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;


/**
 * @author XDEV Software
 *
 */
public class ReplaceabelEditor
{
	private FilterEntryEditor original;
	private FilterEntryEditor copy;
	HorizontalLayout          labelLayout = new HorizontalLayout();

	public ReplaceabelEditor(final FilterEntryEditor editor)
	{
		this.original = editor;
	}
	
	/**
	 * @return the original
	 */
	public FilterEntryEditor getOriginal()
	{
		return this.original;
	}
	
	/**
	 * @return the copy
	 */
	public FilterEntryEditor getCopy()
	{
		return this.copy;
	}
	
	public void updateCopy()
	{
		this.copy = FilterEntryEditor.copyEditor(this.original);
	}
	
	public void setOriginal(final FilterEntryEditor editor)
	{
		this.original = FilterEntryEditor.copyEditor(editor);
	}

	public void setLabelLayout(final HorizontalLayout layout)
	{
		this.labelLayout = layout;
	}

	public HorizontalLayout getLabelLayout()
	{
		return this.labelLayout;
	}
	
}

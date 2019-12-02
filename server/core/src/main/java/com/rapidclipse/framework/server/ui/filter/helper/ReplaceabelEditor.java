
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;


/**
 * @author XDEV Software<br>
 *         <br>
 *         Object which holds 2 instances of {@link FilterEntryEditor}.<br>
 *         One is the original and one the copy.<br>
 *         It also has the finalLayout and the Label EntryRow<br>
 *         This class is needed to switch between Orignal and Copy and also update the needed Layout and/or EntryRow
 */
public class ReplaceabelEditor implements Replaceabel
{
	private FilterEntryEditor original;
	private FilterEntryEditor copy;
	HorizontalLayout          labelLayout = new HorizontalLayout();
	EntryRowLabel             entryRow    = new EntryRowLabel();

	public ReplaceabelEditor(final FilterEntryEditor editor)
	{
		this.original = editor;

	}

	/**
	 * @return The original-object -> {@link FilterEntryEditor}
	 */
	@Override
	public FilterEntryEditor getOriginal()
	{
		return this.original;
	}

	/**
	 * @return The Copy-object -> {@link FilterEntryEditor}
	 */
	public FilterEntryEditor getCopy()
	{
		return this.copy;
	}

	/**
	 * Make a deep copy of the original-object into the copy-object<br>
	 * Both are type {@link FilterEntryEditor}
	 */
	@Override
	public void updateCopy()
	{
		this.copy = FilterEntryEditor.copyEditor(this.original);
	}

	/**
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 */
	public void setOriginal(final FilterEntryEditor editor)
	{
		this.original = FilterEntryEditor.copyEditor(editor);
	}

	/**
	 *
	 * @param layout
	 *            -> {@link HorizontalLayout}
	 */
	@Override
	public void setLabelLayout(final HorizontalLayout layout)
	{
		this.labelLayout = layout;
	}

	/**
	 *
	 * @return The Label -> {@link HorizontalLayout}
	 */
	@Override
	public HorizontalLayout getLabelLayout()
	{
		return this.labelLayout;
	}

	/**
	 * @return the entryRow
	 */
	public EntryRowLabel getEntryRow()
	{
		return this.entryRow;
	}

	/**
	 * @param entryRow
	 *            -> {@link EntryRowLabel}
	 */
	@Override
	public void setEntryRow(final EntryRowLabel entryRow)
	{
		this.entryRow = entryRow;
	}
	
	/**
	 * The Copy will now be the new Original.<br>
	 * The Copy will also be updated with a new deep Copy of herself.
	 */
	@Override
	public void updateOriginalWithCopy()
	{
		setOriginal(getCopy());
		updateCopy();
	}
	
}

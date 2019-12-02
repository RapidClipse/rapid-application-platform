
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.ui.filter.helper.interfaces.FilterComponentInterface;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;


/**
 * @author XDEV Software
 *
 */
public class LabelButtons
{
	private final FilterCheckBox           checkbox;
	private final EditButton               editButton;
	private final DeleteButton             deleteButton;
	private final FilterComponentInterface component;

	public LabelButtons(final FilterComponentInterface component)
	{
		this.checkbox     = new FilterCheckBox();
		this.editButton   = new EditButton();
		this.deleteButton = new DeleteButton();
		this.component    = component;
	}
	
	void definingButtons(final Replaceabel editor)
	{
		this.checkbox.defineCheckBox();
		this.checkbox.setValueChangeListener(this.component, editor);

		this.editButton.defineButton();
		this.editButton.setClickListener(this.component, editor);

		this.deleteButton.defineButton();
		this.deleteButton.setClickListener(this.component, editor);

	}
	
	/********* Getter/ Setter *********/

	/**
	 * @return the checkbox
	 */
	public FilterCheckBox getCheckbox()
	{
		return this.checkbox;
	}

	/**
	 * @return the editButton
	 */
	public EditButton getEditButton()
	{
		return this.editButton;
	}

	/**
	 * @return the deleteButton
	 */
	public DeleteButton getDeleteButton()
	{
		return this.deleteButton;
	}

}

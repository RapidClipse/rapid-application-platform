
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.ui.filter.FilterComponent;


/**
 * @author XDEV Software
 *
 */
public class LabelButtons
{
	private FilterCheckBox  checkbox;
	private EditButton      editButton;
	private DeleteButton    deleteButton;
	private FilterComponent component;

	public LabelButtons(final FilterComponent component)
	{
		this.checkbox     = new FilterCheckBox();
		this.editButton   = new EditButton();
		this.deleteButton = new DeleteButton();
		this.component    = component;
	}
	
	/**
	 * @param checkbox
	 * @param editButton
	 * @param deleteButton
	 */
	public LabelButtons(final FilterCheckBox checkbox, final EditButton editButton, final DeleteButton deleteButton)
	{
		super();
		this.checkbox     = checkbox;
		this.editButton   = editButton;
		this.deleteButton = deleteButton;
	}
	
	public void definingButtons(final ReplaceabelEditor editor)
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
	 * @param checkbox
	 *            the checkbox to set
	 */
	public void setCheckbox(final FilterCheckBox checkbox)
	{
		this.checkbox = checkbox;
	}
	
	/**
	 * @return the editButton
	 */
	public EditButton getEditButton()
	{
		return this.editButton;
	}
	
	/**
	 * @param editButton
	 *            the editButton to set
	 */
	public void setEditButton(final EditButton editButton)
	{
		this.editButton = editButton;
	}
	
	/**
	 * @return the deleteButton
	 */
	public DeleteButton getDeleteButton()
	{
		return this.deleteButton;
	}
	
	/**
	 * @param deleteButton
	 *            the deleteButton to set
	 */
	public void setDeleteButton(final DeleteButton deleteButton)
	{
		this.deleteButton = deleteButton;
	}
	
}

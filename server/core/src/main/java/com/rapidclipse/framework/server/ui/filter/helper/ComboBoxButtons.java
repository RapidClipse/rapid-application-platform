
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.ui.filter.FilterComponent;


/**
 * @author XDEV Software
 *
 */
public class ComboBoxButtons
{
	private UpdateButton    updateButton;
	private CancelButton    cancelButton;
	private FilterComponent component;

	/**
	 *
	 */
	public ComboBoxButtons(final FilterComponent component)
	{
		super();
		this.updateButton = new UpdateButton();
		this.cancelButton = new CancelButton();
		this.component    = component;
	}

	/**
	 * @param updateButton
	 * @param cancelButton
	 * @param component
	 */
	public ComboBoxButtons(
		final UpdateButton updateButton,
		final CancelButton cancelButton,
		final FilterComponent component)
	{
		super();
		this.updateButton = updateButton;
		this.cancelButton = cancelButton;
		this.component    = component;
	}
	
	public void definingButtons(final ReplaceabelEditor editor)
	{
		this.cancelButton.defineButton();
		this.cancelButton.setClickListener(this.component, editor);

		this.updateButton.defineButton();
		this.updateButton.setClickListener(this.component, editor);
	}
	
	/**
	 * @return the updateButton
	 */
	public UpdateButton getUpdateButton()
	{
		return this.updateButton;
	}

	/**
	 * @param updateButton
	 *            the updateButton to set
	 */
	public void setUpdateButton(final UpdateButton updateButton)
	{
		this.updateButton = updateButton;
	}

	/**
	 * @return the cancelButton
	 */
	public CancelButton getCancelButton()
	{
		return this.cancelButton;
	}

	/**
	 * @param cancelButton
	 *            the cancelButton to set
	 */
	public void setCancelButton(final CancelButton cancelButton)
	{
		this.cancelButton = cancelButton;
	}

	/**
	 * @return the component
	 */
	public FilterComponent getComponent()
	{
		return this.component;
	}

	/**
	 * @param component
	 *            the component to set
	 */
	public void setComponent(final FilterComponent component)
	{
		this.component = component;
	}

}

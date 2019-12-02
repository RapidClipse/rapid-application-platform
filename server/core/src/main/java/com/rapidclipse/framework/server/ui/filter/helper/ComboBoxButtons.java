
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.ui.filter.helper.interfaces.FilterComponentInterface;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;


/**
 * @author XDEV Software
 *
 */
public class ComboBoxButtons
{
	private final UpdateButton             updateButton;
	private final CancelButton             cancelButton;
	private final FilterComponentInterface component;
	
	/**
	 *
	 */
	public ComboBoxButtons(final FilterComponentInterface component)
	{
		super();
		this.updateButton = new UpdateButton();
		this.cancelButton = new CancelButton();
		this.component    = component;
	}

	public void definingButtons(final Replaceabel editor)
	{
		this.cancelButton.defineButton();
		this.cancelButton.setClickListener(this.component, editor);
		
		this.updateButton.defineButton();
		this.updateButton.setClickListener(this.component, editor);
	}
	
	/********* Getter/ Setter *********/
	
	/**
	 * @return the updateButton
	 */
	public UpdateButton getUpdateButton()
	{
		return this.updateButton;
	}
	
	/**
	 * @return the cancelButton
	 */
	public CancelButton getCancelButton()
	{
		return this.cancelButton;
	}
	
}

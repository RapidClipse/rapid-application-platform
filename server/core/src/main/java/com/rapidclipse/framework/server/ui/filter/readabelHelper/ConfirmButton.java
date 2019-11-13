
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class ConfirmButton extends Button
{
	
	public void defineButton()
	{
		this.setClassName("confirmButton");
		this.setIcon(VaadinIcon.CHECK.create());
	}
}

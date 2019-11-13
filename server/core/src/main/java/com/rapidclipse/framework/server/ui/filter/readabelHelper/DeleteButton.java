
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class DeleteButton extends Button
{
	public void defineButton()
	{
		this.setClassName("deleteButton");
		this.setIcon(VaadinIcon.MINUS.create());
	}

}


package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class EditButton extends Button
{
	
	public void defineButton()
	{
		this.setClassName("editButton");
		this.setIcon(VaadinIcon.EDIT.create());
	}
}

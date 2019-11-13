
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class UpdateButton extends Button
{
	public void defineButton()
	{
		this.setIcon(VaadinIcon.CHECK.create());
		this.setClassName("addButton");
	}
}

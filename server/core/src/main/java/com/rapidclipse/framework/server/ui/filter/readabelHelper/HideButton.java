
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class HideButton extends Button
{
	boolean open;
	Icon    icon;
	
	/**
	 * Set true or false. Either if something is open or not
	 *
	 * @param b
	 */
	public void setOpen(final boolean b)
	{
		this.open = b;
		
	}
	
	/**
	 * return the boolean <b> open</b> to check if something is open or not
	 *
	 * @return
	 */
	public boolean isOpen()
	{
		return this.open;
	}

	public void defineButton()
	{
		this.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
		setOpen(false);
		this.addClassName("hideFilterButton");
	}
	
}

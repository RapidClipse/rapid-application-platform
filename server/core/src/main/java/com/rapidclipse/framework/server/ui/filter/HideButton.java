
package com.rapidclipse.framework.server.ui.filter;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.Icon;


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

}

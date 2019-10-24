
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
	 * @param b
	 */
	public void setOpen(final boolean b)
	{
		this.open = b;

	}

	public boolean isOpen()
	{
		return this.open;
	}
	
}


package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.vaadin.flow.component.button.Button;


/**
 * @author XDEV Software
 *
 */
public abstract class Buttons<T> extends Button
{
	
	/**
	 * Defines the Button with specific Values like Classname, etc.
	 */
	public abstract void defineButton();

	/**
	 * Adds a Clicklistener to the Button
	 *
	 * @param first
	 *            -> First Interface or Object to handle with
	 * @param second
	 *            -> Second Interface or Object to handle with
	 */
	public abstract void setClickListener(T first, Replaceabel second);
}

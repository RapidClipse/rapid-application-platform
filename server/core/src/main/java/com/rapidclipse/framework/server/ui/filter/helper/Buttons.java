
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.ui.filter.FilterComponent;
import com.vaadin.flow.component.button.Button;


/**
 * @author XDEV Software
 *
 */
public abstract class Buttons extends Button
{
	
	/**
	 * Defines the Button with specific Values like Classname, etc.
	 */
	public abstract void defineButton();
	
	/**
	 * Adds a Clicklistener to the Button
	 *
	 * @param component
	 *            -> {@link FilterComponent}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	public abstract void setClickListener(FilterComponent component, ReplaceabelEditor editor);
}

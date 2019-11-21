
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.vaadin.flow.component.html.Div;


/**
 * @author XDEV Software
 *
 */
public class LabelDiv extends Div
{
	/**
	 * Defining the Div which is used to hold the labels beyond the Comboboxes
	 *
	 * Classname = labelDiv -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	public void defineDiv()
	{
		this.setVisible(false);
		this.setWidthFull();
		this.addClassName(StringResourceUtils.getResourceString("labelDiv", this));
	}
}

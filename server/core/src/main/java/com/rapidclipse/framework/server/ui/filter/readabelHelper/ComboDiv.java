
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.vaadin.flow.component.html.Div;


/**
 * @author XDEV Software
 *
 */
public class ComboDiv extends Div
{

	/**
	 * Defines the ComboDiv with ClassName, etc.
	 * <br>
	 * Classname = comboBoxDiv -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 *
	 */
	public void defineDiv()
	{
		this.setVisible(true);
		this.setWidthFull();
		this.addClassName(StringResourceUtils.getResourceString("comboBoxDiv", this));
	}
	
}

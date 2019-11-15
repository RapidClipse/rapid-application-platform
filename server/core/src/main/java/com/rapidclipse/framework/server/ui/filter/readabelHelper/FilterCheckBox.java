
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.vaadin.flow.component.checkbox.Checkbox;


/**
 * @author XDEV Software
 *
 */
public class FilterCheckBox extends Checkbox
{
	/**
	 * Defines the Checkbox with Classname, etc.
	 *
	 * Classname = checkBox -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	public void defineCheckBox()
	{
		this.setClassName(StringResourceUtils.getResourceString("checkBox", this));
		this.setValue(true);
	}
}

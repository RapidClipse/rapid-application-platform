
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.vaadin.flow.component.checkbox.Checkbox;


/**
 * @author XDEV Software
 *
 */
public class FilterCheckBox extends Checkbox
{
	
	public void defineCheckBox()
	{
		this.setClassName("checkBox");
		this.setValue(true);
	}
}

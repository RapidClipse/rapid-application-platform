
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.vaadin.flow.component.html.Div;


/**
 * @author XDEV Software
 *
 */
public class FilterDiv extends Div
{
	/**
	 * Defines the FilterDiv with ClassName, etc.
	 * <br>
	 * Classname = filterDiv -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 *
	 * @param div
	 *            -> {@link Div}
	 */
	public void defineDiv()
	{
		this.setClassName(StringResourceUtils.getResourceString("filterDiv", this));
		this.setVisible(false);
		this.setEnabled(true);
		this.setSizeFull();
	}
	
	/**
	 * Opens this {@link Div} by clicking on the given {@link HideButton}
	 * <br>
	 * Also setting 'open' in the {@link HideButton} to true
	 *
	 * @param button
	 *            -> {@link HideButton}
	 */
	public void openDiv(final HideButton button)
	{
		button.open();
		this.setVisible(true);
		
	}
	
}

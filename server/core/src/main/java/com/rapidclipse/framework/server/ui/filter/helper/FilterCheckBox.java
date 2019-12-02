
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterComponent;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.ToggleFilter;
import com.vaadin.flow.component.checkbox.Checkbox;


/**
 * @author XDEV Software
 *
 */
public class FilterCheckBox extends Checkbox
{
	private static final String TITLE = "title";
	
	/**
	 * Defines the Checkbox with Classname, etc.
	 *
	 * Classname = checkBox -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	public void defineCheckBox()
	{
		this.setClassName(StringResourceUtils.getResourceString("checkBox", this));
		this.getElement().setProperty(TITLE, StringResourceUtils.getResourceString("checkboxTrueHover", this));
		this.setValue(true);
	}
	
	public void setActive()
	{
		this.getElement().setProperty(TITLE, StringResourceUtils.getResourceString("checkboxTrueHover", this));
	}
	
	public void setNonActive()
	{
		this.getElement().setProperty(TITLE, StringResourceUtils.getResourceString("checkboxFalseHover", this));
	}
	
	/**
	 * Set a ValueChangeListener to the Checkbox
	 *
	 * @param component
	 *            -> {@link FilterComponent}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	public void setValueChangeListener(final ToggleFilter filter, final Replaceabel editor)
	{
		this.addValueChangeListener(listener -> {
			
			final Boolean check = this.getValue();
			if(Boolean.TRUE.equals(check))
			{
				this.setActive();
				filter.activateFilter(editor);
			}
			else if(Boolean.FALSE.equals(check))
			{
				this.setNonActive();
				filter.deactivateFilter(editor);
			}
			
		});
	}
}

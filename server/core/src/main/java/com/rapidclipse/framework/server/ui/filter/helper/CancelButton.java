
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterComponent;
import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class CancelButton extends Buttons
{
	
	/**
	 * Defines the Button with Classname, etc.
	 *
	 * Classname = cancelButton -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	@Override
	public void defineButton()
	{
		this.setClassName(StringResourceUtils.getResourceString("cancelButton", this));
		this.setIcon(VaadinIcon.BAN.create());
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);
		this.getElement().setProperty("title", StringResourceUtils.getResourceString("cancelHover", this));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setClickListener(final FilterComponent component, final ReplaceabelEditor editor)
	{
		this.addClickListener(listener -> {
			editor.updateOriginalWithCopy();
			component.newFilterEntry();
			component.updateFilterData();
		});

	}

	/**
	 * Creates the ClickListener for this {@link Button}. This method just removes the current Filter inside the
	 * <b>comboDiv</b> and add a new one. <br>
	 * This method is only used if the current Filter is a <b>new</b> one and <b>not</b> added to the
	 * <b>filterEntryEditors List</b> yet.
	 *
	 * @param component
	 *            -> {@link FilterComponent}
	 */
	public void setClickListener(final FilterComponent component)
	{
		this.addClickListener(listener -> {
			component.comboDiv.removeAll();
			component.addFilterEntryEditor(new FilterEntryEditor(component, component, component::updateFilterData));
		});
	}
}

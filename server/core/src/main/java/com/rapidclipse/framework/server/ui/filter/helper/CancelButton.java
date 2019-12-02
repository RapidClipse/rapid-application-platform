
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterComponent;
import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.UpdateFilterData;
import com.vaadin.flow.component.HasComponents;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class CancelButton extends Buttons<UpdateFilterData>
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
	public void setClickListener(final UpdateFilterData component, final Replaceabel editor)
	{
		this.addClickListener(listener -> {
			editor.updateOriginalWithCopy();
			component.newFilterEntry();
			component.updateFilterData();
		});
	}

	public void setClickListener(final HasComponents obj, final FilterComponent component)
	{
		this.addClickListener(listener -> {
			obj.removeAll();
			component.addFilterEntryEditor(new FilterEntryEditor(component, component, component::updateFilterData));
		});
	}
}

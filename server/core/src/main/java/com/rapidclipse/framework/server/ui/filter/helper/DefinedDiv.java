
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;


/**
 * @author XDEV Software
 *
 */
public class DefinedDiv extends Div
{

	/**
	 * Creates the the Button layout for the given {@link Component}s
	 *
	 * @param components
	 *            -> As many {@link Component} as necessary
	 * @return -> {@link HorizontalLayout}
	 */
	public HorizontalLayout createButtonLayout(final Component... components)
	{
		final HorizontalLayout layout = createLayout(components);
		
		layout.addClassName(StringResourceUtils.getResourceString("buttonLayout", this));
		
		return layout;
	}

	/**
	 * Creates the Layout which is then seen from the User inside a Div
	 *
	 * @param filterEntryRow
	 *            -> The left part of the Layout with the needed Editor as {@link Component}
	 *            <br>
	 *            {@link #createEntryRowCombo(FilterEntryEditor)}
	 * @param finalButtonLayout
	 *            -> The right part of the Layout with the needed Buttons as {@link Component}
	 *            <br>
	 *            Can be created with {@link #createButtonLayout(Component, Component, Component)} or
	 *            {@link #createButtonLayout(Component, Component)}
	 * @return The final Layout as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = finalLayout
	 */
	public HorizontalLayout createFinalLayout(final Component... components)
	{
		final HorizontalLayout layout = createLayout(components);
		
		layout.addClassName(StringResourceUtils.getResourceString("finalLayout", this));
		
		return layout;
	}
	
	private HorizontalLayout createLayout(final Component... components)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		
		for(final Component c : components)
		{
			layout.add(c);
		}
		
		return layout;
		
	}
}

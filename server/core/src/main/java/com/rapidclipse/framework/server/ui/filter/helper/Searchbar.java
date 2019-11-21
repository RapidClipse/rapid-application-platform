
package com.rapidclipse.framework.server.ui.filter.helper;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;


/**
 * @author XDEV Software
 *         The Searchbar used to filter a table.
 *
 */
public class Searchbar extends HorizontalLayout
{
	/**
	 * Defining the Searchbar
	 */
	public void defineSearchbar()
	{
		this.setMargin(false);
		this.setPadding(false);
		this.setWidth("100%");
	}
	
	/**
	 * Adding all given Components to the Searchbar.
	 * The searchTextField will be expand.
	 *
	 * @param searchTextField
	 *            -> {@link Component}
	 * @param hideButton
	 *            -> {@link Component}
	 * @param addButton
	 *            -> {@link Component}
	 */
	public void createSearchBar(final Component searchTextField, final Component hideButton, final Component addButton)
	{
		
		this.add(searchTextField, hideButton, addButton);
		this.expand(searchTextField);
	}
}

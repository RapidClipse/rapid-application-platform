
package com.rapidclipse.framework.server.ui.filter;

import java.util.ArrayList;
import java.util.List;

import com.vaadin.flow.component.html.Label;


/**
 * @author XDEV Software
 *
 */
public class FilterEntryEditorLabel extends Label
{
	List<FilterEntryEditorInterface> listener = new ArrayList<>();
	
	public void addListener(final FilterEntryEditorInterface e)
	{
		this.listener.add(e);
	}
	
}

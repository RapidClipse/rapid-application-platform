
package com.rapidclipse.framework.server.ui.filter.helper.interfaces;

import java.util.List;

import com.rapidclipse.framework.server.ui.filter.FilterContext;
import com.rapidclipse.framework.server.ui.filter.helper.ComboDiv;
import com.rapidclipse.framework.server.ui.filter.helper.LabelDiv;
import com.vaadin.flow.component.HasSize;


/**
 * @author XDEV Software
 *
 */
public interface FilterComponentInterface
	extends UpdateFilterData, ToggleFilter, RemoveFilterEditor, FilterContext, HasSize
{
	LabelDiv getLabelDiv();
	
	ComboDiv getComboDiv();
	
	List<Replaceabel> getFilterEntryEditors();
}

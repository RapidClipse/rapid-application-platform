
package com.rapidclipse.framework.server.ui.filter.helper.interfaces;

import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.rapidclipse.framework.server.ui.filter.helper.EntryRowLabel;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;


/**
 * @author XDEV Software
 *
 */
public interface Replaceabel
{
	void updateOriginalWithCopy();
	
	FilterEntryEditor getOriginal();
	
	void setEntryRow(EntryRowLabel entry);
	
	void updateCopy();

	void setLabelLayout(HorizontalLayout layout);
	
	HorizontalLayout getLabelLayout();
	
}

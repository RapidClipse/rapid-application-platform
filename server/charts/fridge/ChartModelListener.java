
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.EventListener;


/**
 * @author XDEV Software
 *
 */
public interface ChartModelListener extends Serializable, EventListener
{
	public void onChange(ChartModelChangeEvent event);
}

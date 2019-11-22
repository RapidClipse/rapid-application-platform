
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.EventListener;


/**
 * @author XDEV Software
 *
 */
public interface ChartConfigurationListener<C extends ChartConfiguration<C>> extends Serializable, EventListener
{
	public void onChange(ChartConfigurationChangeEvent<C> event);
}

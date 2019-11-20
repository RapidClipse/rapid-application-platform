
package com.rapidclipse.framework.server.charts;

import java.util.UUID;

import org.apache.commons.lang3.StringUtils;

import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.html.Div;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */

@JavaScript("https://www.gstatic.com/charts/loader.js")
public abstract class Chart extends Composite<Div> implements HasSize
{
	protected Chart()
	{
		super();
	}
	
	protected String id()
	{
		String id = getId().orElse(null);
		if(StringUtils.isEmpty(id))
		{
			setId(id = UUID.randomUUID().toString().replace("-", ""));
		}
		return id;
	}
}

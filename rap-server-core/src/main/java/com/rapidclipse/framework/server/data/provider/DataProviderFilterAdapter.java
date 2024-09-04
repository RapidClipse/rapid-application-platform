/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.provider;

import java.io.Serializable;

import com.rapidclipse.framework.server.data.filter.Filter;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;


/**
 * @author XDEV Software
 *
 */
public interface DataProviderFilterAdapter<D extends ConfigurableFilterDataProvider<?, ?, ?>> extends Serializable
{
	public boolean supports(ConfigurableFilterDataProvider<?, ?, ?> dataProvider);
	
	public void updateFilter(D dataProvider, Filter filter);
}

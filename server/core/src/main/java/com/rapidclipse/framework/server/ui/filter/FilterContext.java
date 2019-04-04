/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.filter;

import com.rapidclipse.framework.server.data.filter.Composite.Connector;


/**
 * @author XDEV Software
 *
 */
public interface FilterContext
{
	/**
	 *
	 * @return if the search should be case sensitive
	 *
	 * @see SimpleStringFilter#isIgnoreCase()
	 */
	public boolean isCaseSensitive();
	
	/**
	 *
	 * @return the character which is used as wildcard in search terms
	 */
	public char getWildcard();
	
	/**
	 *
	 * @return the connector for the searched properties of the container
	 */
	public Connector getSearchPropertiesConnector();
	
	/**
	 *
	 * @return the connector for each word in a multi word search of the search
	 *         term
	 */
	public Connector getSearchMultiWordConnector();
	
	/**
	 *
	 * @return the connector for the properties of filter condition
	 */
	public Connector getFilterPropertiesConnector();
	
	/**
	 *
	 * @return the connector for the search term and the filter condition
	 */
	public Connector getSearchAndFilterConnector();
	
	public FilterOperatorRegistry getFilterOperatorRegistry();
	
	public SubsetDataProviderFactoryRegistry getSubsetDataProviderFactoryRegistry();
	
	public FilterSubject getFilterSubject();
}

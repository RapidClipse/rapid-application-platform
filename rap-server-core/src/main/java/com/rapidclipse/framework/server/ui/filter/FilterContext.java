/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
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

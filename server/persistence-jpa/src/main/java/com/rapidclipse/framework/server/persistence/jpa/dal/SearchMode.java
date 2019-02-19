/*-
 * ---
 * Rapid Application Platform / Server / Persistence / JPA
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.persistence.jpa.dal;

/**
 * Static values to use in conjunction with {@link SearchParameters} object. It
 * maps the kind of search you can do in SQL.
 *
 * @author XDEV Software
 */
public enum SearchMode
{
	/**
	 * Match exactly the properties
	 */
	EQUALS("eq"),
	/**
	 * Activates LIKE search and add a '%' prefix and suffix before searching.
	 */
	ANYWHERE("any"),
	/**
	 * Activate LIKE search and add a '%' prefix before searching.
	 */
	STARTING_LIKE("sl"),
	/**
	 * Activate LIKE search. User provides the wildcard.
	 */
	LIKE("li"),
	/**
	 * Activate LIKE search and add a '%' suffix before searching.
	 */
	ENDING_LIKE("el");
	
	private final String code;
	
	SearchMode(final String code)
	{
		this.code = code;
	}
	
	public String getCode()
	{
		return this.code;
	}
	
	public static final SearchMode convert(final String code)
	{
		for(final SearchMode searchMode : SearchMode.values())
		{
			if(searchMode.getCode().equals(code))
			{
				return searchMode;
			}
		}
		
		return EQUALS; // default
	}
}

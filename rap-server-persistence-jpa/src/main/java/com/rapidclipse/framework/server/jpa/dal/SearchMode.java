/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa.dal;

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
	EQUALS,
	/**
	 * Activates LIKE search and add a '%' prefix and suffix before searching.
	 */
	ANYWHERE,
	/**
	 * Activate LIKE search and add a '%' prefix before searching.
	 */
	STARTING_LIKE,
	/**
	 * Activate LIKE search. User provides the wildcard.
	 */
	LIKE,
	/**
	 * Activate LIKE search and add a '%' suffix before searching.
	 */
	ENDING_LIKE;
}

/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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

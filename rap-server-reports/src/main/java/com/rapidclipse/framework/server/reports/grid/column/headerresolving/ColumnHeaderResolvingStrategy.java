/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.reports.grid.column.headerresolving;

import java.util.Optional;

import com.vaadin.flow.component.grid.Grid.Column;


/**
 * Can be implemented to create a new strategy that resolves a {@link Column} header
 * 
 * @author XDEV Software
 *
 */
public interface ColumnHeaderResolvingStrategy
{
	/**
	 * Resolves the text for a column header
	 *
	 * @param column
	 *            The column for which the header text should be resolved
	 * @return
	 *         {@link Optional#empty()} when the text could not be resolved and the next function should be used.<br/>
	 *         If a value was found the Optional contains the string.
	 */
	Optional<String> resolve(Column<?> column);
}

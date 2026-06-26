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
 * 
 * Uses Vaadin's {@link Column#getHeaderText()}<br/>
 * This will resolve headers who have been set using {@link Column#setHeader(String)}.<br/>
 * 
 * @author XDEV Software
 *
 */
public class VaadinColumnHeaderResolvingStrategy implements ColumnHeaderResolvingStrategy
{
	
	@Override
	public Optional<String> resolve(final Column<?> column)
	{
		try
		{
			return Optional.ofNullable(column.getHeaderText());
		}
		catch(final Exception e)
		{
			return Optional.empty();
		}
	}
	
}

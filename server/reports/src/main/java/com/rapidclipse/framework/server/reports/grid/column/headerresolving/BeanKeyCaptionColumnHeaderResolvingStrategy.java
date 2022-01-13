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

package com.rapidclipse.framework.server.reports.grid.column.headerresolving;

import java.util.Optional;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;


/**
 * Resolves the header by calling {@link CaptionUtils#resolveCaption(Class, String)} for the bean type of the
 * {@link Grid}.<br/>
 * Prior to version 11 the default function.
 * 
 * @author XDEV Software
 * 
 */
public class BeanKeyCaptionColumnHeaderResolvingStrategy implements ColumnHeaderResolvingStrategy
{
	
	@Override
	public Optional<String> resolve(final Column<?> column)
	{
		final String key = column.getKey();
		if(key == null)
		{
			return Optional.empty();
		}
		
		final Class<?> beanType = column.getGrid().getBeanType();
		if(beanType == null)
		{
			return Optional.empty();
		}
		
		try
		{
			final String caption = CaptionUtils.resolveCaption(beanType, column.getKey());
			if(!StringUtils.isEmpty(caption))
			{
				return Optional.of(caption);
			}
		}
		catch(final Exception e)
		{
			// swallow
		}
		
		return Optional.empty();
	}
	
}

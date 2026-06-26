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

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.reports.grid.column;

import java.util.function.Consumer;

import com.vaadin.flow.component.grid.Grid.Column;


/**
 * Used for (initially) building a {@link ColumnConfiguration}
 * 
 * @author XDEV Software
 *
 */
public class ColumnConfigurationBuilder
{
	private final ColumnConfigurationHeaderResolvingStrategyBuilder columnConfigHeaderResolvingStrategyBuilder =
		new ColumnConfigurationHeaderResolvingStrategyBuilder();
	
	public ColumnConfigurationBuilder withColumnConfigHeaderResolvingStrategyBuilder(
		final Consumer<ColumnConfigurationHeaderResolvingStrategyBuilder> configureBuilderFunc)
	{
		configureBuilderFunc.accept(this.columnConfigHeaderResolvingStrategyBuilder);
		return this;
	}
	
	public <T> ColumnConfiguration<T> build(final Column<T> gridColumn)
	{
		return new ColumnConfiguration<>(gridColumn, this.columnConfigHeaderResolvingStrategyBuilder.build());
	}
}

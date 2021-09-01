
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

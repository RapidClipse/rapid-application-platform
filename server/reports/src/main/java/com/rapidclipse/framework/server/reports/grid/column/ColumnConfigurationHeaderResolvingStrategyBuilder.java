
package com.rapidclipse.framework.server.reports.grid.column;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import com.rapidclipse.framework.server.reports.grid.column.headerresolving.BeanKeyCaptionColumnHeaderResolvingStrategy;
import com.rapidclipse.framework.server.reports.grid.column.headerresolving.ColumnHeaderResolvingStrategy;
import com.rapidclipse.framework.server.reports.grid.column.headerresolving.ManualColumnHeaderResolvingStrategy;
import com.rapidclipse.framework.server.reports.grid.column.headerresolving.VaadinInternalRenderingColumnHeaderResolvingStrategy;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.function.SerializableFunction;


/**
 * Builds a function (from multiple strategies) that resolves the text for a column.<br/>
 * If no strategies are specified the fallback {@link Column#getKey()} is used.
 * 
 * @author XDEV Software
 *
 */
public class ColumnConfigurationHeaderResolvingStrategyBuilder
{
	private final List<ColumnHeaderResolvingStrategy> strategies = new ArrayList<>();
	
	/**
	 * Uses the {@link VaadinInternalRenderingColumnHeaderResolvingStrategy}
	 *
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder withVaadinInternalHeaderStrategy()
	{
		return withStrategy(new VaadinInternalRenderingColumnHeaderResolvingStrategy());
	}
	
	/**
	 * Uses the {@link BeanKeyCaptionColumnHeaderResolvingStrategy}
	 * 
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder withBeanKeyCaptionStrategy()
	{
		return withStrategy(new BeanKeyCaptionColumnHeaderResolvingStrategy());
	}
	
	/**
	 * Uses the {@link ManualColumnHeaderResolvingStrategy}
	 * 
	 * @return
	 */
	public <I> ColumnConfigurationHeaderResolvingStrategyBuilder withManualColumnHeaderStrategy(
		final Function<Column<?>, I> identifierResolver,
		final Map<I, Function<I, String>> headerTextResolverMap)
	{
		return withStrategy(new ManualColumnHeaderResolvingStrategy<>(identifierResolver, headerTextResolverMap));
	}
	
	/**
	 * Adds a new {@link ColumnConfigHeaderResolvingStrategy}.<br/>
	 * This strategy will be added at the end of the strategy list.
	 * 
	 * @param strategy
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder withStrategy(final ColumnHeaderResolvingStrategy strategy)
	{
		this.strategies.add(strategy);
		return this;
	}
	
	/**
	 * Adds a new {@link ColumnConfigHeaderResolvingStrategy}.<br/>
	 * This strategy will be added at the start of the strategy list.
	 * 
	 * @param strategy
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder
		withFirstStrategy(final ColumnHeaderResolvingStrategy strategy)
	{
		this.strategies.add(0, strategy);
		return this;
	}
	
	/**
	 * Clears all existing strategies
	 *
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder clearAllStrategies()
	{
		this.strategies.clear();
		return this;
	}
	
	public SerializableFunction<Column<?>, String> build()
	{
		return col -> {
			for(final ColumnHeaderResolvingStrategy resolvingFunction : this.strategies)
			{
				final Optional<String> optResolvedValue = resolvingFunction.resolve(col);
				if(optResolvedValue.isPresent())
				{
					return optResolvedValue.get();
				}
			}
			
			// Fallback
			if(col.getKey() != null)
			{
				return col.getKey();
			}
			return "";
		};
	}
}

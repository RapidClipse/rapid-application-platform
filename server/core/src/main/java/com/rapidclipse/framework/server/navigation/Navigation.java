
package com.rapidclipse.framework.server.navigation;

import static java.util.Objects.requireNonNull;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.router.RouteData;


/**
 * @author XDEV Software
 *
 */
public interface Navigation
{
	public static Navigation To(final Class<?> targetType)
	{
		return To(UI.getCurrent(), targetType);
	}
	
	public static Navigation To(final UI ui, final Class<?> targetType)
	{
		return new Implementation(ui, findComponentTargetType(ui, targetType));
	}
	
	public static void navigateTo(final Class<?> targetType)
	{
		To(targetType).navigate();
	}
	
	public static void rerouteTo(final BeforeEvent event, final Class<?> targetType)
	{
		rerouteTo(event, UI.getCurrent(), targetType);
	}
	
	public static void rerouteTo(final BeforeEvent event, final UI ui, final Class<?> targetType)
	{
		event.rerouteTo(findComponentTargetType(ui, targetType));
	}
	
	public static Class<? extends Component> findComponentTargetType(
		final UI ui,
		final Class<?> targetType)
	{
		final RouteData targetRoute = ui.getRouter().getRoutes().stream()
			.filter(data -> targetType.equals(data.getNavigationTarget())).findAny()
			.orElse(ui.getRouter().getRoutes().stream()
				.filter(data -> targetType.isAssignableFrom(data.getNavigationTarget()))
				.findAny().orElse(null));
		if(targetRoute != null)
		{
			return targetRoute.getNavigationTarget();
		}

		throw new NavigationException("No route found for: " + targetType.getCanonicalName());
	}
	
	public Navigation withParameter(String name, final Object value);
	
	public void navigate();
	
	public static class Implementation implements Navigation
	{
		private final UI                           ui;
		private final Class<? extends Component>   targetType;
		private final NavigationParametersMetadata metadata;
		private final Map<String, Object>          parameters;
		
		public Implementation(final UI ui, final Class<? extends Component> targetType)
		{
			super();
			this.ui         = requireNonNull(ui);
			this.targetType = requireNonNull(targetType);
			this.metadata   = NavigationParametersMetadata.New(targetType);
			this.parameters = new HashMap<>();
		}
		
		@Override
		public Navigation withParameter(String name, final Object value)
		{
			name = requireNonNull(name).toLowerCase();
			
			final NavigationParameterMetadata paramMetadata = this.metadata.get(name);
			if(paramMetadata == null)
			{
				throw new IllegalArgumentException("Parameter " + this.targetType.getCanonicalName()
					+ "#" + name + " not found");
			}
			
			if(value != null && !paramMetadata.type().isInstance(value))
			{
				throw new IllegalArgumentException(name + " = " + value);
			}
			
			this.parameters.put(name, value);
			
			return this;
		}
		
		@Override
		public void navigate()
		{
			final List<String> mandatoryParameters = this.metadata.mandatoryParameters();
			mandatoryParameters.removeAll(this.parameters.keySet());
			if(mandatoryParameters.size() > 0)
			{
				throw new NavigationException("Missing parameters: "
					+ mandatoryParameters.stream().collect(Collectors.joining(", ")));
			}
			
			if(HasNavigationParameters.class.isAssignableFrom(this.targetType))
			{
				final NavigationParameterRegistry registry = NavigationParameterRegistry
					.getCurrent();
				final String                      id       = registry.put(NavigationParameters.New(this.parameters));
				
				final Map<String, String>         paramMap = new HashMap<>();
				paramMap.put(NavigationUtils.ID_PARAMETER_NAME, id);
				
				// _ as dummy parameter value
				final String url = this.ui.getRouter().getUrl(targetTypeWithParameters(), "_");
				
				this.ui.navigate(url, QueryParameters.simple(paramMap));
			}
			else
			{
				this.ui.navigate(this.targetType);
			}
		}
		
		@SuppressWarnings("unchecked") // Type-safety ensured by condition
		private <T extends Component & HasNavigationParameters> Class<T> targetTypeWithParameters()
		{
			return (Class<T>)this.targetType;
		}
	}
}

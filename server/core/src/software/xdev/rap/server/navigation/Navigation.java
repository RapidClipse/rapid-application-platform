/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.navigation;


import static java.util.Objects.requireNonNull;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.QueryParameters;


/**
 * @author XDEV Software
 *
 */
public interface Navigation
{
	public static Navigation To(final Class<? extends Component> targetType)
	{
		return To(UI.getCurrent(),targetType);
	}
	
	
	public static Navigation To(final UI ui, final Class<? extends Component> targetType)
	{
		return new Implementation(ui,targetType);
	}
	
	
	public Navigation withParameter(String name, final Object value);
	
	
	public void navigate();
	
	
	
	public static class Implementation implements Navigation
	{
		private final UI							ui;
		private final Class<? extends Component>	targetType;
		private final NavigationParametersMetadata	metadata;
		private final Map<String, Object>			parameters;
		
		
		public Implementation(final UI ui, final Class<? extends Component> targetType)
		{
			super();
			this.ui = requireNonNull(ui);
			this.targetType = requireNonNull(targetType);
			this.metadata = NavigationParametersMetadata.New(targetType);
			this.parameters = new HashMap<>();
		}
		
		
		@Override
		public Navigation withParameter(String name, final Object value)
		{
			requireNonNull(name);

			name = name.toLowerCase();
			
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
			
			this.parameters.put(name,value);
			
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
						.get(this.ui.getSession());
				final String id = registry.put(NavigationParameters.New(this.parameters));
				
				final Map<String, String> paramMap = new HashMap<>();
				paramMap.put(NavigationUtils.ID_PARAMETER_NAME,id);
				
				// _ as dummy parameter value
				final String url = this.ui.getRouter().getUrl(targetTypeWithParameters(),"_");
				
				this.ui.navigate(url,QueryParameters.simple(paramMap));
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

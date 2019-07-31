/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.ui.action;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.googlecode.gentyref.GenericTypeReflector;
import com.rapidclipse.framework.server.Rap;
import com.rapidclipse.framework.server.ui.action.Action.ContextSensitive;
import com.vaadin.flow.component.ClickNotifier;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 *
 */
public final class ActionRegistry implements Serializable
{
	public static ActionRegistry getCurrent()
	{
		return Rap.ensureSessionInstance(ActionRegistry.class, ActionRegistry::new);
	}

	public static <C extends Component & ClickNotifier<?>> void
		connect(final C component, final Class<? extends Action> actionType)
	{
		getCurrent().getAction(actionType).connectWith(component);
	}
	
	private final static List<Class<? extends Action>>                              actionTypes          =
		new ArrayList<>();
	private final static Map<Class<? extends Action.ContextSensitive<?>>, Class<?>> actionContextMapping =
		new HashMap<>();
	
	static void registerActionType(final Class<? extends Action> type)
	{
		actionTypes.add(type);
	}
	
	private static Class<?> getContextType(final Class<? extends Action.ContextSensitive<?>> contextSensitiveType)
	{
		if(!actionContextMapping.containsKey(contextSensitiveType))
		{
			actionContextMapping.put(contextSensitiveType, resolveContextType(contextSensitiveType));
		}
		return actionContextMapping.get(contextSensitiveType);
	}
	
	private static Class<?> resolveContextType(final Class<? extends Action.ContextSensitive<?>> contextSensitiveType)
	{
		Type type = GenericTypeReflector.getExactSuperType(contextSensitiveType, ContextSensitive.class);
		if(type instanceof ParameterizedType)
		{
			final Type[] typeArguments = ((ParameterizedType)type).getActualTypeArguments();
			if(typeArguments.length == 1)
			{
				type = typeArguments[0];
				while(type instanceof ParameterizedType)
				{
					type = ((ParameterizedType)type).getRawType();
				}
				if(type instanceof Class)
				{
					return (Class<?>)type;
				}
			}
		}
		throw new IllegalStateException(
			contextSensitiveType.getName() + " implements " + ContextSensitive.class.getName()
				+ " without type parameter");
	}
	
	private final Map<Class<? extends Action>, Action> registry       = new HashMap<>();
	private final Map<Class<?>, Object>                activeContexts = new HashMap<>();
	
	private ActionRegistry(final VaadinSession session)
	{
		super();
		
		for(final Class<? extends Action> type : actionTypes)
		{
			try
			{
				final Action action = type.newInstance();
				this.registry.put(type, action);
			}
			catch(InstantiationException | IllegalAccessException e)
			{
				throw new RuntimeException(e);
			}
		}
	}
	
	@SuppressWarnings("unchecked")
	public <A extends Action> A getAction(final Class<A> type)
	{
		return (A)this.registry.get(type);
	}
	
	void updateActions(final List<HasElement> activeChain)
	{
		this.activeContexts.clear();
		
		this.registry.values().stream()
			.filter(Action.ContextSensitive.class::isInstance)
			.map(Action.ContextSensitive.class::cast)
			.forEach(action -> {
				
				final Class<?> type = getContextType(action);
				final Object context = activeChain.stream()
					.filter(type::isInstance)
					.map(type::cast)
					.findFirst()
					.orElse(null);
				if(context != null)
				{
					this.activeContexts.put(type, context);
					action.setEnabled(true);
				}
				else
				{
					action.setEnabled(false);
				}
			});
	}
	
	@SuppressWarnings("unchecked")
	<C> C getContext(final Action.ContextSensitive<?> action)
	{
		return (C)this.activeContexts.get(getContextType(action));
	}
	
	@SuppressWarnings("unchecked")
	private Class<?> getContextType(final Action.ContextSensitive<?> action)
	{
		return ActionRegistry.getContextType((Class<? extends ContextSensitive<?>>)action.getClass());
	}
}

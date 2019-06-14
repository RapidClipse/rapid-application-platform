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

package com.rapidclipse.framework.server.ui;

import com.vaadin.flow.component.AbstractField.ComponentValueChangeEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.data.binder.Setter;
import com.vaadin.flow.function.ValueProvider;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public interface ReadOnlyHasValueAdapter<C extends Component, V> extends HasValue<ComponentValueChangeEvent<C, V>, V>
{
	@Override
	public default Registration
		addValueChangeListener(final ValueChangeListener<? super ComponentValueChangeEvent<C, V>> listener)
	{
		return null;
	}

	@Override
	public default void setReadOnly(final boolean readOnly)
	{
	}

	@Override
	public default boolean isReadOnly()
	{
		return true;
	}

	@Override
	public default void setRequiredIndicatorVisible(final boolean requiredIndicatorVisible)
	{
	}

	@Override
	public default boolean isRequiredIndicatorVisible()
	{
		return false;
	}
	
	public static <C extends Component, V> ReadOnlyHasValueAdapter<C, V>
		New(final C component, final ValueProvider<C, V> getter, final Setter<C, V> setter)
	{
		return new Implementation<>(component, getter, setter);
	}
	
	public static class Implementation<C extends Component, V> implements ReadOnlyHasValueAdapter<C, V>
	{
		private final C                   component;
		private final ValueProvider<C, V> getter;
		private final Setter<C, V>        setter;

		protected Implementation(final C component, final ValueProvider<C, V> getter, final Setter<C, V> setter)
		{
			super();

			this.component = component;
			this.getter    = getter;
			this.setter    = setter;
		}

		@Override
		public void setValue(final V value)
		{
			this.setter.accept(this.component, value);
		}

		@Override
		public V getValue()
		{
			return this.getter.apply(this.component);
		}
	}
}

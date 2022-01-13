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
		return new Default<>(component, getter, setter);
	}

	public static class Default<C extends Component, V> implements ReadOnlyHasValueAdapter<C, V>
	{
		private final C                   component;
		private final ValueProvider<C, V> getter;
		private final Setter<C, V>        setter;
		
		protected Default(final C component, final ValueProvider<C, V> getter, final Setter<C, V> setter)
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

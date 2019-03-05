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

package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;

import com.vaadin.flow.component.HasValidation;
import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.HasValueAndElement;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 *
 */
public interface FilterValueEditorComposite<PRESENTATION, MODEL> extends Serializable
{
	public HasValueAndElement<?, PRESENTATION> component();
	
	public MODEL getValue();
	
	public void setValue(MODEL value);
	
	public static <PRESENTATION, MODEL> FilterValueEditorComposite<PRESENTATION, MODEL> New(
		final HasValueAndElement<?, PRESENTATION> component,
		final Converter<PRESENTATION, MODEL> converter)
	{
		return new Implementation<>(component, converter);
	}
	
	public static <MODEL> FilterValueEditorComposite<MODEL, MODEL> New(
		final HasValueAndElement<?, MODEL> component)
	{
		return new Implementation<>(component, Converter.identity());
	}
	
	public static class Implementation<PRESENTATION, MODEL>
		implements FilterValueEditorComposite<PRESENTATION, MODEL>
	{
		private final HasValueAndElement<?, PRESENTATION>         component;
		private final Binder<Implementation<PRESENTATION, MODEL>> binder;
		private MODEL                                             value;
		
		public Implementation(
			final HasValueAndElement<?, PRESENTATION> component,
			final Converter<PRESENTATION, MODEL> converter)
		{
			super();
			
			this.component = component;
			
			this.binder = new Binder<Implementation<PRESENTATION, MODEL>>()
			{
				@Override
				protected void handleError(
					final HasValue<?, ?> field,
					final ValidationResult result)
				{
					if(field instanceof HasValidation)
					{
						final HasValidation fieldWithValidation = (HasValidation)field;
						fieldWithValidation.setInvalid(true);
					}
				}
			};
			this.binder.forField(component).withConverter(converter).bind(
				Implementation<PRESENTATION, MODEL>::getValue,
				Implementation<PRESENTATION, MODEL>::setModelValue);
			this.binder.setBean(this);
		}
		
		@Override
		public HasValueAndElement<?, PRESENTATION> component()
		{
			return this.component;
		}
		
		@Override
		public MODEL getValue()
		{
			return this.value;
		}
		
		@Override
		public void setValue(final MODEL value)
		{
			this.value = value;
			binder.readBean(this);
		}
		
		private void setModelValue(final MODEL value)
		{
			this.value = value;
		}
	}
}

/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
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
public interface FilterValueEditorComposite<PRESENTATION, MODEL> extends Serializable, Cloneable
{
	public HasValueAndElement<?, PRESENTATION> component();
	
	public MODEL getValue();
	
	public void setValue(MODEL value);
	
	public MODEL copie();

	public static <PRESENTATION, MODEL> FilterValueEditorComposite<PRESENTATION, MODEL> New(
		final HasValueAndElement<?, PRESENTATION> component,
		final Converter<PRESENTATION, MODEL> converter)
	{
		return new Default<>(component, converter);
	}
	
	public static <MODEL> FilterValueEditorComposite<MODEL, MODEL> New(
		final HasValueAndElement<?, MODEL> component)
	{
		return new Default<>(component, Converter.identity());
	}
	
	public static class Default<PRESENTATION, MODEL>
		implements FilterValueEditorComposite<PRESENTATION, MODEL>
	{
		private final HasValueAndElement<?, PRESENTATION>  component;
		private final Binder<Default<PRESENTATION, MODEL>> binder;
		private MODEL                                      value;
		
		protected Default(
			final HasValueAndElement<?, PRESENTATION> component,
			final Converter<PRESENTATION, MODEL> converter)
		{
			super();
			
			this.component = component;
			
			this.binder = new Binder<Default<PRESENTATION, MODEL>>()
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
				Default<PRESENTATION, MODEL>::getValue,
				Default<PRESENTATION, MODEL>::setModelValue);
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
			this.binder.readBean(this);
		}
		
		private void setModelValue(final MODEL value)
		{
			this.value = value;
		}
		
		/*
		 * (non-Javadoc)
		 * 
		 * @see com.rapidclipse.framework.server.ui.filter.FilterValueEditorComposite#copie()
		 */
		@SuppressWarnings("unchecked")
		@Override
		public MODEL copie()
		{
			MODEL clone = null;
			try
			{
				clone = (MODEL)super.clone();
			}
			catch(final CloneNotSupportedException cns)
			{
				System.out.println("Something went wrong, while copying");
			}
			return clone;
		}
		
	}
	
}

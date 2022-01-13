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
package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;

import com.vaadin.flow.component.HasValidation;
import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.HasValueAndElement;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
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
		final Converter<PRESENTATION, MODEL> converter,
		final PRESENTATION nullRepresentation)
	{
		return new Default<>(component, converter, nullRepresentation);
	}

	public static <MODEL> FilterValueEditorComposite<MODEL, MODEL> New(
		final HasValueAndElement<?, MODEL> component,
		final MODEL nullRepresentation)
	{
		return new Default<>(component, Converter.identity(), nullRepresentation);
	}

	public static class Default<PRESENTATION, MODEL>
		implements FilterValueEditorComposite<PRESENTATION, MODEL>
	{
		private final HasValueAndElement<?, PRESENTATION>  component;
		private final Binder<Default<PRESENTATION, MODEL>> binder;
		private MODEL                                      value;

		protected Default(
			final HasValueAndElement<?, PRESENTATION> component,
			final Converter<PRESENTATION, MODEL> converter,
			final PRESENTATION nullRepresentation)
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

				@Override
				public void setBean(final Default<PRESENTATION, MODEL> bean)
				{
					super.setBean(bean);
				}
			};
			this.binder.forField(component)
				.withConverter(new NullAwareConverter<>(converter, nullRepresentation))
				.bind(
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
		
		private static class NullAwareConverter<PRESENTATION, MODEL> implements Converter<PRESENTATION, MODEL>
		{
			private final Converter<PRESENTATION, MODEL> delegate;
			private final PRESENTATION                   nullRepresentation;

			NullAwareConverter(
				final Converter<PRESENTATION, MODEL> delegate,
				final PRESENTATION nullRepresentation)
			{
				super();
				this.delegate           = delegate;
				this.nullRepresentation = nullRepresentation;
			}

			@Override
			public Result<MODEL> convertToModel(final PRESENTATION value, final ValueContext context)
			{
				return this.delegate.convertToModel(value, context);
			}

			@Override
			public PRESENTATION convertToPresentation(final MODEL value, final ValueContext context)
			{
				final PRESENTATION presentation = this.delegate.convertToPresentation(value, context);
				return presentation != null
					? presentation
					: this.nullRepresentation;
			}

		}

	}

}

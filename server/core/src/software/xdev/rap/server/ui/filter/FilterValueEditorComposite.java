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

package software.xdev.rap.server.ui.filter;


import com.vaadin.flow.component.HasValueAndElement;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 *
 */
public interface FilterValueEditorComposite<PRESENTATION, MODEL>
{
	public HasValueAndElement<?, PRESENTATION> component();


	public MODEL getValue();


	public void setValue(MODEL value);


	public static <PRESENTATION, MODEL> FilterValueEditorComposite<PRESENTATION, MODEL> New(
			final HasValueAndElement<?, PRESENTATION> component,
			final Converter<PRESENTATION, MODEL> converter)
	{
		return new WithConverter<>(component,converter);
	}


	public static <MODEL> FilterValueEditorComposite<MODEL, MODEL> New(
			final HasValueAndElement<?, MODEL> component)
	{
		return new WithoutConverter<>(component);
	}



	public static abstract class Abstract<PRESENTATION, MODEL>
			implements FilterValueEditorComposite<PRESENTATION, MODEL>
	{
		private final HasValueAndElement<?, PRESENTATION>	component;
		private MODEL										value;


		public Abstract(final HasValueAndElement<?, PRESENTATION> component)
		{
			super();

			this.component = component;
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
		}
	}



	public static class WithConverter<PRESENTATION, MODEL> extends Abstract<PRESENTATION, MODEL>
	{
		@SuppressWarnings("unchecked")
		public WithConverter(final HasValueAndElement<?, PRESENTATION> component,
				final Converter<PRESENTATION, MODEL> converter)
		{
			super(component);

			new Binder<>(getClass()).forField(component).withConverter(converter)
					.bind(c -> (MODEL)c.getValue(),(c, v) -> c.setValue(v));
		}
	}



	public static class WithoutConverter<MODEL> extends Abstract<MODEL, MODEL>
	{
		@SuppressWarnings("unchecked")
		public WithoutConverter(final HasValueAndElement<?, MODEL> component)
		{
			super(component);

			new Binder<>(getClass()).forField(component).bind(c -> (MODEL)c.getValue(),
					(c, v) -> c.setValue(v));
		}
	}
}

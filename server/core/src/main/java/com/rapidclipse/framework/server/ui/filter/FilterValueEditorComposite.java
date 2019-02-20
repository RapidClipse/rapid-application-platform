
package com.rapidclipse.framework.server.ui.filter;

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
public interface FilterValueEditorComposite<PRESENTATION, MODEL>
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
			
			this.binder    = new Binder<Implementation<PRESENTATION, MODEL>>()
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

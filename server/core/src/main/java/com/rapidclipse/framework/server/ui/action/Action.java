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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.flowingcode.vaadin.addons.ironicons.IronIconEnum;
import com.vaadin.flow.component.ClickNotifier;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentEvent;
import com.vaadin.flow.component.HasEnabled;
import com.vaadin.flow.component.HasText;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.KeyModifier;
import com.vaadin.flow.component.ShortcutEventListener;
import com.vaadin.flow.component.ShortcutRegistration;
import com.vaadin.flow.component.Shortcuts;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.IconFactory;
import com.vaadin.flow.function.SerializableSupplier;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public interface Action extends Serializable
{
	public String getText();
	
	public Action setText(String text);
	
	public SerializableSupplier<Component> getIcon();
	
	public Action setIcon(SerializableSupplier<Component> icon);
	
	public default Action setIcon(final IconFactory factory)
	{
		return setIcon((SerializableSupplier<Component>)factory::create);
	}
	
	public default Action setIcon(final IronIconEnum ironIcon)
	{
		return setIcon((SerializableSupplier<Component>)ironIcon::create);
	}
	
	public Key getShortcutKey();
	
	public Action setShortcutKey(Key shortcutKey);
	
	public Collection<KeyModifier> getShortcutKeyModifiers();
	
	public Action setShortcutKeyModifiers(Collection<KeyModifier> shortcutKeyModifiers);
	
	public default Action setShortcutKeyModifiers(final KeyModifier... shortcutKeyModifiers)
	{
		setShortcutKeyModifiers(Arrays.asList(shortcutKeyModifiers));
		return this;
	}
	
	public default Action setShortcut(final Key shortcutKey, final KeyModifier... shortcutKeyModifiers)
	{
		return setShortcutKey(shortcutKey).setShortcutKeyModifiers(shortcutKeyModifiers);
	}
	
	public boolean isEnabled();
	
	public Action setEnabled(boolean b);
	
	public void execute(ComponentEvent<?> event);
	
	public <C extends Component & ClickNotifier<?>> Registration connectWith(C component);
	
	public static abstract class Abstract implements Action
	{
		private String                                     text;
		private SerializableSupplier<Component>            icon;
		private Key                                        shortcutKey;
		private Collection<KeyModifier>                    shortcutKeyModifiers;
		private boolean                                    enabled;
		private final Map<Component, ShortcutRegistration> connectedComponents = new HashMap<>();
		
		protected Abstract()
		{
			super();
		}
		
		@Override
		public String getText()
		{
			return this.text;
		}
		
		@Override
		public Action setText(final String text)
		{
			if(!Objects.equals(this.text, text))
			{
				this.text = text;
				updateConnectedComponents();
			}
			
			return this;
		}
		
		@Override
		public SerializableSupplier<Component> getIcon()
		{
			return this.icon;
		}
		
		@Override
		public Action setIcon(final SerializableSupplier<Component> icon)
		{
			if(!Objects.equals(this.icon, icon))
			{
				this.icon = icon;
				updateConnectedComponents();
			}
			
			return this;
		}
		
		@Override
		public Key getShortcutKey()
		{
			return this.shortcutKey;
		}
		
		@Override
		public Action setShortcutKey(final Key shortcutKey)
		{
			if(!Objects.equals(this.shortcutKey, shortcutKey))
			{
				this.shortcutKey = shortcutKey;
				updateConnectedComponents();
			}
			return this;
		}
		
		@Override
		public Collection<KeyModifier> getShortcutKeyModifiers()
		{
			return this.shortcutKeyModifiers != null
				? this.shortcutKeyModifiers
				: Collections.emptyList();
		}
		
		@Override
		public Action setShortcutKeyModifiers(final Collection<KeyModifier> shortcutKeyModifiers)
		{
			if(!Objects.equals(this.shortcutKeyModifiers, shortcutKeyModifiers))
			{
				this.shortcutKeyModifiers = shortcutKeyModifiers != null
					? new ArrayList<>(shortcutKeyModifiers)
					: Collections.emptyList();
				updateConnectedComponents();
			}
			return this;
		}
		
		@Override
		public boolean isEnabled()
		{
			return this.enabled;
		}
		
		@Override
		public Action setEnabled(final boolean enabled)
		{
			if(this.enabled != enabled)
			{
				this.enabled = enabled;
				updateConnectedComponents();
			}
			return this;
		}
		
		@Override
		public <C extends Component & ClickNotifier<?>> Registration connectWith(final C component)
		{
			final Registration clickRegistration = component.addClickListener(Abstract.this::execute);
			
			this.connectedComponents.put(component, null);
			
			updateComponent(component);
			
			return () -> {
				
				final ShortcutRegistration shortcutRegistration = this.connectedComponents.remove(component);
				if(shortcutRegistration != null)
				{
					shortcutRegistration.remove();
					clickRegistration.remove();
				}
			};
		}
		
		protected void updateConnectedComponents()
		{
			this.connectedComponents.keySet().forEach(this::updateComponent);
		}
		
		protected void updateComponent(final Component component)
		{
			if(component instanceof HasText)
			{
				((HasText)component).setText(getText());
			}
			
			// Interface HasIcon still missing
			if(component instanceof Button)
			{
				final Button                          button  = (Button)component;
				final SerializableSupplier<Component> factory = getIcon();
				final Component                       icon    = factory != null ? factory.get() : null;
				if(button.getIcon() != icon)
				{
					button.setIcon(icon);
				}
			}
			
			if(component instanceof HasEnabled)
			{
				((HasEnabled)component).setEnabled(isEnabled());
			}
			
			final ShortcutRegistration shortcutRegistration = this.connectedComponents.get(component);
			if(shortcutRegistration == null)
			{
				updateShortcutRegistration(component);
			}
			else if(!equalsShortcut(shortcutRegistration))
			{
				shortcutRegistration.remove();
				updateShortcutRegistration(component);
			}
		}
		
		protected boolean equalsShortcut(final ShortcutRegistration reg)
		{
			return Objects.equals(this.shortcutKey, reg.getKey()) &&
				Arrays.equals(getShortcutKeyModifiers().toArray(), reg.getModifiers().toArray());
		}
		
		protected void updateShortcutRegistration(final Component component)
		{
			final Key shortcutKey = getShortcutKey();
			if(shortcutKey != null)
			{
				final ShortcutRegistration registration =
					Shortcuts.addShortcutListener(component, (ShortcutEventListener)event -> {
						execute(new ComponentEvent<Component>(event.getSource(), false));
					}, shortcutKey, getShortcutKeyModifiers().stream().toArray(KeyModifier[]::new));
				registration.setBrowserDefaultAllowed(false);
				this.connectedComponents.put(component, registration);
			}
		}
	}
	
	public static interface ContextSensitive<C> extends Action
	{
		public default C getContext()
		{
			return ActionRegistry.getCurrent().getContext(this);
		}
	}
}

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.littemplate.LitTemplate;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;


/**
 * This class allows subclasses to register consumers for various javascript events. These are distinguished by their
 * consumed type.
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public abstract class JavascriptTemplate extends LitTemplate
{
	private final Map<Class<?>, Set<? extends SerializableConsumer<?>>> consumers = new HashMap<>();

	protected JavascriptTemplate()
	{
		super();
	}
	
	protected JavascriptTemplate(final HasElement parent)
	{
		super();
		
		parent.getElement().appendVirtualChild(this.getElement());
	}

	/**
	 * Add a new consumer to the set of consumers for the specified type.
	 * The consumer can be remove with the returned registration.
	 *
	 * @param type
	 *            Class type of the consumed value.
	 * @param consumer
	 *            The consumer that should be added to the consumer set.
	 */
	protected <T extends Serializable> Registration
		registerConsumer(final Class<T> type, final SerializableConsumer<T> consumer)
	{
		return registerConsumer(type, consumer, null, null);
	}

	/**
	 * Add a new consumer to the set of consumers for the specified type.
	 * The consumer can be remove with the returned registration.
	 *
	 * @param type
	 *            Class type of the consumed value.
	 * @param firstAddedCallback
	 *            called before the first consumer for the type is registered
	 * @param lastRemovedCallback
	 *            called after the last consumer for the type was removed
	 * @param consumer
	 *            The consumer that should be added to the consumer set.
	 */
	@SuppressWarnings("unchecked")
	protected <T extends Serializable> Registration registerConsumer(
		final Class<T> type,
		final SerializableConsumer<T> consumer,
		final Runnable firstAddedCallback,
		final Runnable lastRemovedCallback)
	{
		synchronized(this.consumers)
		{
			((Set<SerializableConsumer<T>>)this.consumers.computeIfAbsent(type, t -> {
				if(firstAddedCallback != null)
				{
					firstAddedCallback.run();
				}
				return new HashSet<>();
			})).add(consumer);
		}
		return () -> {
			synchronized(this.consumers)
			{
				final Set<? extends SerializableConsumer<?>> consumers = this.consumers.get(type);
				if(consumers != null)
				{
					consumers.remove(consumer);
					if(consumers.isEmpty())
					{
						this.consumers.remove(type);
						if(lastRemovedCallback != null)
						{
							lastRemovedCallback.run();
						}
					}
				}
			}
		};
	}

	protected <T extends Serializable> void unregisterAllConsumers(final Class<T> type)
	{
		unregisterAllConsumers(type, null);
	}

	protected <T extends Serializable> void
		unregisterAllConsumers(final Class<T> type, final Runnable unregisterCallback)
	{
		synchronized(this.consumers)
		{
			final Set<? extends SerializableConsumer<?>> consumers = this.consumers.get(type);
			if(consumers != null && !consumers.isEmpty())
			{
				consumers.clear();
				this.consumers.remove(type);
				if(unregisterCallback != null)
				{
					unregisterCallback.run();
				}
			}
		}
	}

	/**
	 * Notify all the consumers of the given class type to consume the given value.
	 *
	 * @param type
	 *            The class type of the consumers that should be notified.
	 * @param object
	 *            The object that should be consumed by the consumers.
	 */
	@SuppressWarnings("unchecked")
	protected <T extends Serializable> void notifyConsumers(final Class<T> type, final T object)
	{
		synchronized(this.consumers)
		{
			Optional.ofNullable(this.consumers.get(type))
				.ifPresent(set -> ((Set<SerializableConsumer<T>>)set).forEach(c -> c.accept(object)));
		}
	}

	/**
	 * Clear the consumers map.
	 */
	protected void clearConsumers()
	{
		this.consumers.clear();
	}
}

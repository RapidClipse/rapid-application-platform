
package com.rapidclipse.framework.server.ui.filter;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public class Ob<T>
{
	private final Set<Consumer<T>> listeners = ConcurrentHashMap.newKeySet();

	public Registration register(final Consumer<T> listener)
	{
		this.listeners.add(listener);

		return () -> this.listeners.remove(listener);
	}

	public void sendEvent(final T event)
	{
		this.listeners.forEach(c -> c.accept(event));
	}
}

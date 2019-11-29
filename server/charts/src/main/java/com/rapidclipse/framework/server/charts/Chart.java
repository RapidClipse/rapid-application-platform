
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.server.util.JavaScriptable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.HasSize;


/**
 * @author XDEV Software
 *
 */
public interface Chart extends HasElement, HasSize
{
	public ChartModel getModel();

	public Properties properties();

	public static class Properties implements Serializable, JavaScriptable
	{
		private final Map<String, Object>              properties        = new HashMap<>();
		private final Map<String, Map<Object, Object>> indexedProperties = new HashMap<>();

		protected Properties()
		{
			super();
		}

		public <T> void put(final String name, final T value)
		{
			if(value == null)
			{
				this.properties.remove(name);
			}
			else
			{
				this.properties.put(name, value);
			}
		}

		@SuppressWarnings("unchecked")
		public <T> T get(final String name)
		{
			return (T)this.properties.get(name);
		}

		public <T> void putIndexed(final String name, final Object index, final T value)
		{
			this.indexedProperties.computeIfAbsent(name, n -> new HashMap<>()).put(index, value);
		}

		@SuppressWarnings("unchecked")
		public <T> T getIndexed(final String name, final Object index)
		{
			final Map<Object, Object> map = this.indexedProperties.get(name);
			return map != null
				? (T)map.get(index)
				: null;
		}

		@SuppressWarnings("unchecked")
		public <T> T removeIndexed(final String name, final Object index)
		{
			T                         removed = null;
			final Map<Object, Object> map     = this.indexedProperties.get(name);
			if(map != null)
			{
				removed = (T)map.remove(index);
				if(map.isEmpty())
				{
					this.indexedProperties.remove(name);
				}
			}
			return removed;
		}

		public void removeAllIndexed(final String name)
		{
			this.indexedProperties.remove(name);
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			putAll(obj, this.properties);

			this.indexedProperties.entrySet().forEach(ie -> {
				final String              key    = ie.getKey();
				final Map<Object, Object> values = ie.getValue();
				if(values != null && values.size() > 0)
				{
					final ObjectHelper valuesObj = new ObjectHelper();
					putAll(valuesObj, values);
					obj.put(key, valuesObj);
				}
			});

			return obj.js();
		}
		
		private void putAll(final ObjectHelper obj, final Map<?, ?> map)
		{
			map.entrySet().forEach(e -> {
				final Object key   = e.getKey();
				final Object value = e.getValue();
				if(value instanceof Collection)
				{
					obj.putIfNotNull(key.toString(), new ArrayHelper().addAll((Collection<?>)value));
				}
				else
				{
					obj.putIfNotNull(key.toString(), value);
				}
			});
		}
	}
}

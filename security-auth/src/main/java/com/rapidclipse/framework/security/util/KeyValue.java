
package com.rapidclipse.framework.security.util;

/**
 * @author XDEV Software (TM)
 *
 */
public interface KeyValue<K, V>
{
	public K key();
	
	public V value();
	
	public static <K, V> KeyValue<K, V> New(final K key, final V value)
	{
		return new KeyValue.Implementation<>(key, value);
	}
	
	public final class Implementation<K, V> implements KeyValue<K, V>
	{
		final K key;
		final V value;
		
		public Implementation(final K key, final V value)
		{
			super();
			this.key   = key;
			this.value = value;
		}
		
		@Override
		public K key()
		{
			return this.key;
		}
		
		@Override
		public V value()
		{
			return this.value;
		}
		
		/**
		 * @return a String of pattern <code>[<i>key</i> -> <i>value</i>]</code>
		 */
		@Override
		public String toString()
		{
			return '[' + String.valueOf(this.key) + " -> " + String.valueOf(this.value) + ']';
		}
		
	}
	
}

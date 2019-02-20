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

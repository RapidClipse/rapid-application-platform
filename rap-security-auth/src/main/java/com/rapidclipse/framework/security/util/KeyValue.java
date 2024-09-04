/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
		return new Default<>(key, value);
	}

	public final class Default<K, V> implements KeyValue<K, V>
	{
		final K key;
		final V value;

		public Default(final K key, final V value)
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

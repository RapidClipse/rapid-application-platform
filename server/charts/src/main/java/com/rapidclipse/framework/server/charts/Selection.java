
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Selection extends Serializable, JavaScriptable
{
	public interface Item extends Serializable
	{
		public Integer row();
		
		public default boolean hasRow()
		{
			return row() != null;
		}
		
		public Integer column();
		
		public default boolean hasColumn()
		{
			return column() != null;
		}
		
		public static class Default implements Item
		{
			private final Integer row;
			private final Integer column;
			
			Default(final Integer row, final Integer column)
			{
				super();
				
				this.row    = row;
				this.column = column;
			}
			
			@Override
			public Integer row()
			{
				return this.row;
			}
			
			@Override
			public Integer column()
			{
				return this.column;
			}
			
			@Override
			public boolean equals(final Object obj)
			{
				if(obj == this)
				{
					return true;
				}
				
				if(obj instanceof Item)
				{
					final Item other = (Item)obj;
					return Objects.equals(this.row, other.row())
						&& Objects.equals(this.column, other.column());
				}
				
				return false;
			}
		}
	}
	
	public List<Item> items();
	
	public default boolean isEmpty()
	{
		return items().isEmpty();
	}
	
	public static Item Item(final Integer row, final Integer column)
	{
		return new Item.Default(row, column);
	}
	
	public static Selection New(final List<Item> items)
	{
		return new Selection.Default(items);
	}
	
	public static Selection New(final Item... items)
	{
		return new Selection.Default(Arrays.asList(items));
	}
	
	public static Selection Empty()
	{
		return new Selection.Default(Collections.emptyList());
	}
	
	public static Selection Single(final Integer row, final Integer column)
	{
		return New(Item(row, column));
	}
	
	public static class Default implements Selection
	{
		private final List<Item> items;
		
		Default(final List<Item> items)
		{
			super();
			
			this.items = Collections.unmodifiableList(items);
		}
		
		@Override
		public List<Item> items()
		{
			return this.items;
		}
		
		@Override
		public String js()
		{
			final ArrayHelper array = new ArrayHelper();
			for(final Item item : this.items)
			{
				final ObjectHelper obj = new ObjectHelper();
				obj.putIfNotNull("row", item.row());
				obj.putIfNotNull("column", item.column());
				array.add(obj);
			}
			return array.js();
		}
		
		@Override
		public boolean equals(final Object obj)
		{
			if(obj == this)
			{
				return true;
			}
			
			if(obj instanceof Selection)
			{
				final Selection other = (Selection)obj;
				return this.items.equals(other.items());
			}
			
			return false;
		}
	}
}

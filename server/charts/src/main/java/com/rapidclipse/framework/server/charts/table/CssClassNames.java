
package com.rapidclipse.framework.server.charts.table;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface CssClassNames extends Serializable, JavaScriptable
{
	public String headerRow();
	
	public String tableRow();
	
	public String oddTableRow();
	
	public String selectedTableRow();
	
	public String hoverTableRow();
	
	public String headerCell();
	
	public String tableCell();
	
	public String rowNumberCell();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder headerRow(String headerRow);
		
		public Builder tableRow(String tableRow);
		
		public Builder oddTableRow(String oddTableRow);
		
		public Builder selectedTableRow(String selectedTableRow);
		
		public Builder hoverTableRow(String hoverTableRow);
		
		public Builder headerCell(String headerCell);
		
		public Builder tableCell(String tableCell);
		
		public Builder rowNumberCell(String rowNumberCell);
		
		public CssClassNames build();
		
		public static class Default implements Builder
		{
			private String headerRow;
			private String tableRow;
			private String oddTableRow;
			private String selectedTableRow;
			private String hoverTableRow;
			private String headerCell;
			private String tableCell;
			private String rowNumberCell;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder headerRow(final String headerRow)
			{
				this.headerRow = headerRow;
				return this;
			}
			
			@Override
			public Builder tableRow(final String tableRow)
			{
				this.tableRow = tableRow;
				return this;
			}
			
			@Override
			public Builder oddTableRow(final String oddTableRow)
			{
				this.oddTableRow = oddTableRow;
				return this;
			}
			
			@Override
			public Builder selectedTableRow(final String selectedTableRow)
			{
				this.selectedTableRow = selectedTableRow;
				return this;
			}
			
			@Override
			public Builder hoverTableRow(final String hoverTableRow)
			{
				this.hoverTableRow = hoverTableRow;
				return this;
			}
			
			@Override
			public Builder headerCell(final String headerCell)
			{
				this.headerCell = headerCell;
				return this;
			}
			
			@Override
			public Builder tableCell(final String tableCell)
			{
				this.tableCell = tableCell;
				return this;
			}
			
			@Override
			public Builder rowNumberCell(final String rowNumberCell)
			{
				this.rowNumberCell = rowNumberCell;
				return this;
			}
			
			@Override
			public CssClassNames build()
			{
				return new CssClassNames.Default(this.headerRow, this.tableRow, this.oddTableRow, this.selectedTableRow,
					this.hoverTableRow, this.headerCell, this.tableCell, this.rowNumberCell);
			}
			
		}
		
	}

	public static class Default implements CssClassNames
	{
		private final String headerRow;
		private final String tableRow;
		private final String oddTableRow;
		private final String selectedTableRow;
		private final String hoverTableRow;
		private final String headerCell;
		private final String tableCell;
		private final String rowNumberCell;
		
		Default(
			final String headerRow,
			final String tableRow,
			final String oddTableRow,
			final String selectedTableRow,
			final String hoverTableRow,
			final String headerCell,
			final String tableCell,
			final String rowNumberCell)
		{
			super();

			this.headerRow        = headerRow;
			this.tableRow         = tableRow;
			this.oddTableRow      = oddTableRow;
			this.selectedTableRow = selectedTableRow;
			this.hoverTableRow    = hoverTableRow;
			this.headerCell       = headerCell;
			this.tableCell        = tableCell;
			this.rowNumberCell    = rowNumberCell;
		}

		@Override
		public String headerRow()
		{
			return this.headerRow;
		}
		
		@Override
		public String tableRow()
		{
			return this.tableRow;
		}
		
		@Override
		public String oddTableRow()
		{
			return this.oddTableRow;
		}
		
		@Override
		public String selectedTableRow()
		{
			return this.selectedTableRow;
		}
		
		@Override
		public String hoverTableRow()
		{
			return this.hoverTableRow;
		}
		
		@Override
		public String headerCell()
		{
			return this.headerCell;
		}
		
		@Override
		public String tableCell()
		{
			return this.tableCell;
		}
		
		@Override
		public String rowNumberCell()
		{
			return this.rowNumberCell;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("headerRow", this.headerRow);
			obj.putIfNotNull("tableRow", this.tableRow);
			obj.putIfNotNull("oddTableRow", this.oddTableRow);
			obj.putIfNotNull("selectedTableRow", this.selectedTableRow);
			obj.putIfNotNull("hoverTableRow", this.hoverTableRow);
			obj.putIfNotNull("headerCell", this.headerCell);
			obj.putIfNotNull("tableCell", this.tableCell);
			obj.putIfNotNull("rowNumberCell", this.rowNumberCell);
			return obj.js();
		}
		
	}
	
}

/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Candlestick extends Serializable, JavaScriptable
{
	public Boolean hollowIsRising();
	
	public CandlestickColor fallingColor();
	
	public CandlestickColor risingColor();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder hollowIsRising(Boolean hollowIsRising);
		
		public Builder fallingColor(CandlestickColor fallingColor);
		
		public Builder risingColor(CandlestickColor risingColor);
		
		public Candlestick build();
		
		public static class Default implements Builder
		{
			private Boolean          hollowIsRising;
			private CandlestickColor fallingColor;
			private CandlestickColor risingColor;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder hollowIsRising(final Boolean hollowIsRising)
			{
				this.hollowIsRising = hollowIsRising;
				return this;
			}
			
			@Override
			public Builder fallingColor(final CandlestickColor fallingColor)
			{
				this.fallingColor = fallingColor;
				return this;
			}
			
			@Override
			public Builder risingColor(final CandlestickColor risingColor)
			{
				this.risingColor = risingColor;
				return this;
			}
			
			@Override
			public Candlestick build()
			{
				return new Candlestick.Default(this.hollowIsRising, this.fallingColor, this.risingColor);
			}
			
		}
		
	}
	
	public static class Default implements Candlestick
	{
		private final Boolean          hollowIsRising;
		private final CandlestickColor fallingColor;
		private final CandlestickColor risingColor;
		
		Default(
			final Boolean hollowIsRising,
			final CandlestickColor fallingColor,
			final CandlestickColor risingColor)
		{
			super();
			
			this.hollowIsRising = hollowIsRising;
			this.fallingColor   = fallingColor;
			this.risingColor    = risingColor;
		}
		
		@Override
		public Boolean hollowIsRising()
		{
			return this.hollowIsRising;
		}
		
		@Override
		public CandlestickColor fallingColor()
		{
			return this.fallingColor;
		}
		
		@Override
		public CandlestickColor risingColor()
		{
			return this.risingColor;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("hollowIsRising", this.hollowIsRising);
			obj.putIfNotNull("fallingColor", this.fallingColor);
			obj.putIfNotNull("risingColor", this.risingColor);
			return obj.js();
		}
		
	}
	
}

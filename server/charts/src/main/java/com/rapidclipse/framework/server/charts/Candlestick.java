/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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

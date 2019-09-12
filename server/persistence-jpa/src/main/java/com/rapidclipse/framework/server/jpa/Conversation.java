/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.jpa;

import javax.persistence.LockModeType;


/**
 * @author XDEV Software (JW)
 *
 */
public interface Conversation
{
	public boolean isActive();
	
	public boolean isPessimisticUnit();
	
	public void setPessimisticUnit(boolean lockState, LockModeType type);
	
	public LockModeType getLockModeType();
	
	public void setLockModeType(final LockModeType lockModeType);
	
	public void start();
	
	public void end();

	public static Conversation New()
	{
		return new Default();
	}
	
	public class Default implements Conversation
	{
		private boolean      isActive        = false;
		private boolean      pessimisticUnit = false;
		private LockModeType lockModeType;
		
		protected Default()
		{
			super();
		}
		
		@Override
		public LockModeType getLockModeType()
		{
			return this.lockModeType;
		}
		
		@Override
		public void setLockModeType(final LockModeType lockModeType)
		{
			this.lockModeType = lockModeType;
		}
		
		@Override
		public boolean isActive()
		{
			return this.isActive;
		}
		
		@Override
		public void start()
		{
			this.isActive = true;
		}
		
		@Override
		public void end()
		{
			this.isActive = false;
		}
		
		@Override
		public boolean isPessimisticUnit()
		{
			return this.pessimisticUnit;
		}
		
		@Override
		public void setPessimisticUnit(final boolean mode, final LockModeType type)
		{
			this.pessimisticUnit = mode;
			this.lockModeType    = type;
		}
	}
}

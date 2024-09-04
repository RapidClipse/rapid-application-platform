/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.jpa;

import jakarta.persistence.LockModeType;


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

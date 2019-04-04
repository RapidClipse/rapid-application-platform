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
	
	public class Implementation implements Conversation
	{
		private boolean      isActive        = false;
		private boolean      pessimisticUnit = false;
		private LockModeType lockModeType;
		
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

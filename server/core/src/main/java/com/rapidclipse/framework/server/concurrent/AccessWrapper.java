
package com.rapidclipse.framework.server.concurrent;

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public abstract class AccessWrapper
{
	public static interface Participant
	{
		public void before();
		
		public void after();
	}
	
	protected void before()
	{
		ServiceLoader.forType(Participant.class).services().forEach(Participant::before);
	}
	
	protected void after()
	{
		ServiceLoader.forType(Participant.class).services().forEach(Participant::after);
	}
}

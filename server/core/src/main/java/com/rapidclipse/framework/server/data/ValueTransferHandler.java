
package com.rapidclipse.framework.server.data;

/**
 * @author XDEV Software
 *
 */
public interface ValueTransferHandler
{
	public boolean handlesPut(Object value);
	
	public Object put(Object value);
	
	public boolean handlesGet(Object value);
	
	public Object get(Object value);
}

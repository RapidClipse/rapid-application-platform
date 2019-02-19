
package com.rapidclipse.framework.server.mobilekit.accelerometer;

import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public interface AccelerationWatch extends Registration
{
	/**
	 * @return the acceleration
	 */
	public Acceleration getAcceleration();
	
	/**
	 * Stops watching for changes to the device's acceleration
	 */
	@Override
	public void remove();
}

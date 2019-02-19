
package com.rapidclipse.framework.server.mobilekit.geolocation;

import com.vaadin.flow.shared.Registration;


/**
 * Callback object for
 * {@link GeolocationService#watchPosition(GeolocationOptions, java.util.function.Consumer, java.util.function.Consumer)}
 *
 * @author XDEV Software
 *
 */
public interface PositionWatch extends Registration
{
	/**
	 * @return the position
	 */
	public Position getPosition();
	
	/**
	 * Stops watching for changes to the device's location
	 */
	@Override
	public void remove();
}


package com.rapidclipse.framework.server.webapi.permissions;

/**
 * Enumeration of all the permissions that are currently available.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public enum PermissionType
{
	geolocation,
	notifications,
	push,
	midi,
	camera,
	microphone,
	speaker,
	device_info,
	background_fetch,
	background_sync,
	bluetooth,
	persistent_storage,
	ambient_light_sensor,
	accelerometer,
	gyroscope,
	magnetometer,
	clipboard,
	display_catpure,
	nfc;
	
	public static PermissionType fromString(final String str)
	{
		return PermissionType.valueOf(str.replace('-', '_'));
	}
	
	@Override
	public String toString()
	{
		return this.name().replace('_', '-');
	}
}


package com.rapidclipse.framework.server.charts.sankey;

public class Link
{
	private String colorMode = "none";
	
	public String getColorMode()
	{
		return this.colorMode;
	}
	
	/**
	 * Sets a coloring mode for the links between nodes. Possible values:
	 *
	 * <li>'source' - The color of the source node is used for the links to all target nodes.</li>
	 * <li>'target' - The color of the target node is used for the link to its source nodes.</li>
	 * <li>'gradient' - The link between a source and target node is colored as a gradient from the source node color to
	 * the target node color.</li>
	 * <li>'none' - the default option; link colors will be set to the default</li>
	 *
	 * @param colorMode
	 */
	public void setColorMode(final String colorMode)
	{
		this.colorMode = colorMode;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");

		str.append("colorMode: '" + this.colorMode + "' ");
		
		str.append("}");
		
		return str.toString();
	}
	
}

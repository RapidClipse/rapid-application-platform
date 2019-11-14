
package com.rapidclipse.framework.server.charts.sankey;

public class Sankey
{
	private Integer iterations = 32;
	private Link    link;
	private Node    node;
	
	public Integer getIterations()
	{
		return this.iterations;
	}
	
	/**
	 * The larger this number, the more pleasing the layout of complex sankeys
	 * Default: 32
	 *
	 * @param iterations
	 */
	public void setIterations(final Integer iterations)
	{
		this.iterations = iterations;
	}
	
	public Link getLink()
	{
		return this.link;
	}
	
	/**
	 * Controls attributes of the connections between nodes.
	 *
	 * @param link
	 */
	public void setLink(final Link link)
	{
		this.link = link;
	}
	
	public Node getNode()
	{
		return this.node;
	}
	
	/**
	 * Controls attributes of the nodes (the vertical bars between links):
	 *
	 * @param node
	 */
	public void setNode(final Node node)
	{
		this.node = node;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		str.append("iterations: " + this.iterations + ", ");
		str.append("link: " + this.link + ", ");
		str.append("node: " + this.node + " ");
		str.append("}");

		return str.toString();
	}
}

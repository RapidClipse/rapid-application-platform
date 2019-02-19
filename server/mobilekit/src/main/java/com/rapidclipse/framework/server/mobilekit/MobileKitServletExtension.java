
package com.rapidclipse.framework.server.mobilekit;

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.rapidclipse.framework.server.RapServlet;


/**
 * @author XDEV Software
 *
 */
public class MobileKitServletExtension implements RapServlet.Extension
{
	private MobileConfiguration configuration;
	
	public MobileKitServletExtension()
	{
		super();
	}
	
	@Override
	public void servletInitialized(final RapServlet servlet) throws ServletException
	{
		if(this.configuration == null)
		{
			this.configuration = readMobileConfiguration(servlet);
		}
		
		servlet.getService().addSessionInitListener(event -> event.getSession()
			.setAttribute(MobileConfiguration.class, this.configuration));
	}
	
	private MobileConfiguration readMobileConfiguration(final RapServlet servlet)
		throws ServletException
	{
		final List<MobileServiceConfiguration> serviceConfigs = new ArrayList<>();

		try
		{
			final URL url = findMobileXML(servlet);
			if(url == null)
			{
				servlet.log("No mobile.xml found");
			}
			else
			{
				final DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
				final DocumentBuilder        dBuilder  = dbFactory.newDocumentBuilder();
				try(InputStream inputStream = url.openStream())
				{
					final Document doc             = dBuilder.parse(inputStream);
					final Element  documentElement = doc.getDocumentElement();
					if(documentElement != null)
					{
						final ClassLoader classLoader      = getClass().getClassLoader();

						final NodeList    servicesNodeList = documentElement
							.getElementsByTagName("services");
						if(servicesNodeList.getLength() == 1)
						{
							final Element  servicesElement = (Element)servicesNodeList.item(0);
							final NodeList serviceNodeList = servicesElement
								.getElementsByTagName("service");
							for(int i = 0, c = serviceNodeList.getLength(); i < c; i++)
							{
								final Element serviceElement = (Element)serviceNodeList.item(i);
								serviceConfigs.add(
									createServiceConfiguration(classLoader, serviceElement));
							}
						}
					}
				}
			}
		}
		catch(final Exception e)
		{
			throw new ServletException(e);
		}

		return MobileConfiguration.New(serviceConfigs);
	}
	
	@SuppressWarnings("unchecked")
	private MobileServiceConfiguration createServiceConfiguration(
		final ClassLoader classLoader,
		final Element serviceElement)
		throws ServletException
	{
		final String className = serviceElement.getAttribute("class");

		try
		{
			final Class<?> serviceClass = classLoader.loadClass(className);
			if(!MobileService.class.isAssignableFrom(serviceClass))
			{
				throw new IllegalArgumentException(
					className + " is not a " + MobileService.class.getSimpleName());
			}

			final NodeList            paramNodeList = serviceElement.getElementsByTagName("param");
			final Map<String, String> params        = new HashMap<>();
			for(int i = 0, c = paramNodeList.getLength(); i < c; i++)
			{
				final Element paramElement = (Element)paramNodeList.item(i);
				params.put(paramElement.getAttribute("name"), paramElement.getAttribute("value"));
			}

			return MobileServiceConfiguration.New((Class<? extends MobileService>)serviceClass,
				params);
		}
		catch(final Exception e)
		{
			throw new ServletException(e);
		}
	}
	
	private URL findMobileXML(final RapServlet servlet) throws MalformedURLException
	{
		final ClassLoader classLoader = servlet.getService().getClassLoader();
		URL               resourceUrl = classLoader.getResource("META-INF/mobile.xml");
		if(resourceUrl == null)
		{
			resourceUrl = servlet.getServletContext().getResource("/mobile.xml");
			if(resourceUrl == null)
			{
				resourceUrl = classLoader.getResource("mobile.xml");
			}
		}
		return resourceUrl;
	}
}

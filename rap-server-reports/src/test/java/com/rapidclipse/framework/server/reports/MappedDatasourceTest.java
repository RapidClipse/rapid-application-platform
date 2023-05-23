
package com.rapidclipse.framework.server.reports;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.ByteArrayInputStream;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.xml.sax.Attributes;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.data.JsonDataSource;
import net.sf.jasperreports.engine.design.JRDesignField;
import net.sf.jasperreports.engine.xml.JRFieldFactory;
import net.sf.jasperreports.engine.xml.JRXmlConstants;


/**
 * @author XDEV Software
 *
 */
public class MappedDatasourceTest
{
	@Test
	@DisplayName("Get field value from MappedDataSource returns the same as the delegate JsonDataSource")
	void test_001() throws JRException
	{
		final String         initialString  =
			"[{\"firstName\" : \"John\", \"lastName\" : \"Doe\"},{\"firstName\" : \"Jane\", \"lastName\" : \"Smith\"}]";
		final JsonDataSource jsonDataSource = new JsonDataSource(new ByteArrayInputStream(initialString.getBytes()));
		jsonDataSource.next();
		
		final JRDesignField    field            = getField();
		
		final MappedDataSource mappedDataSource = MappedDataSource.create(jsonDataSource, new HashMap<>());
		
		final Object           fieldValue1      = mappedDataSource.getFieldValue(field);
		final Object           fieldValue2      = jsonDataSource.getFieldValue(field);
		
		assertNotNull(fieldValue1);
		assertNotNull(fieldValue2);
		assertEquals(fieldValue1, fieldValue2);
	}
	
	protected JRDesignField getField()
	{
		final Attributes attr = new Attributes()
		{
			Map<String, String> val =
				Map.of(JRXmlConstants.ATTRIBUTE_name, "firstName",
					JRXmlConstants.ATTRIBUTE_class, "java.lang.String");
			
			@Override
			public String getValue(final String uri, final String localName)
			{
				return null;
			}
			
			@Override
			public String getValue(final String qName)
			{
				return this.val.get(qName);
			}
			
			@Override
			public String getValue(final int index)
			{
				return null;
			}
			
			@Override
			public String getURI(final int index)
			{
				return null;
			}
			
			@Override
			public String getType(final String uri, final String localName)
			{
				return null;
			}
			
			@Override
			public String getType(final String qName)
			{
				return null;
			}
			
			@Override
			public String getType(final int index)
			{
				return null;
			}
			
			@Override
			public String getQName(final int index)
			{
				return null;
			}
			
			@Override
			public String getLocalName(final int index)
			{
				return null;
			}
			
			@Override
			public int getLength()
			{
				return 0;
			}
			
			@Override
			public int getIndex(final String uri, final String localName)
			{
				return 0;
			}
			
			@Override
			public int getIndex(final String qName)
			{
				return 0;
			}
		};
		
		return (JRDesignField)new JRFieldFactory().createObject(attr);
	}
}


package com.rapidclipse.framework.server.reports;

import java.io.OutputStream;

import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperPrint;


@FunctionalInterface
public interface PlainExporter
{
	public void export(JasperPrint print, OutputStream stream) throws JRException;
}

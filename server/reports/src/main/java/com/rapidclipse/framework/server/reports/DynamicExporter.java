
package com.rapidclipse.framework.server.reports;

import java.io.OutputStream;

import net.sf.dynamicreports.jasper.builder.JasperReportBuilder;
import net.sf.dynamicreports.report.exception.DRException;


@FunctionalInterface
public interface DynamicExporter
{
	public void export(JasperReportBuilder builder, OutputStream stream) throws DRException;
}

package org.erlide.cover.ui.views.util;

import java.io.StringWriter;
import java.net.URL;
import java.util.Calendar;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.views.model.ICoverageObject;

/**
 * Generates HTML collective reports
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class ReportGenerator {

    private static ReportGenerator instance;

    private VelocityEngine ve;
    private Logger log; // logger

    private ReportGenerator() {
        ve = new VelocityEngine();

        Properties props = new Properties();
        props.put("resource.loader", "file");
        props.put("file.resource.loader.class",
                "org.apache.velocity.runtime.resource.loader.FileResourceLoader");
        props.put("file.resource.loader.path", "");
        props.put("file.resource.loader.cache", "true");

        ve.init(props);
        log = Logger.getLogger(getClass());
        ;
    }

    public static synchronized ReportGenerator getInstance() {
        if (instance == null)
            instance = new ReportGenerator();
        return instance;
    }

    public String getHTMLreport(ICoverageObject obj) {
        // organize data

        String date = Calendar.getInstance().getTime().toString();
        String type = "file"; // TODO
        String cssPath = "";
        try {
        URL bundleRoot = Platform.getBundle(Activator.PLUGIN_ID).getEntry(
            "/templates/reports.css");
        URL fileURL = FileLocator.toFileURL(bundleRoot);
        java.io.File file = new java.io.File(fileURL.toURI());
        cssPath = file.getAbsolutePath();
        } catch(Exception e) {
            e.printStackTrace();
            log.error(e);
        }
        
        // add data to a context
        VelocityContext context = new VelocityContext();
        context.put("obj", obj);
        context.put("children", obj.getChildren());
        context.put("date", date);
        context.put("type", type);
        context.put("css_path", cssPath);

        try {
            URL bundleRoot = Platform.getBundle(Activator.PLUGIN_ID).getEntry(
                    "/templates/report.vm");
            URL fileURL = FileLocator.toFileURL(bundleRoot);
            java.io.File file = new java.io.File(fileURL.toURI());
            Template t = ve.getTemplate(file.getAbsolutePath());

            StringWriter writer = new StringWriter();
            t.merge(context, writer);

            return writer.toString();
        } catch (Exception e) {
            // TODO
            e.printStackTrace();
            return null;
        }

    }
}

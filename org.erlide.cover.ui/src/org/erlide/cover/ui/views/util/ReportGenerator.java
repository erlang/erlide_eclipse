package org.erlide.cover.ui.views.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Calendar;
import java.util.Properties;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.resource.loader.StringResourceLoader;
import org.eclipse.core.runtime.Platform;
import org.erlide.cover.core.Activator;
import org.erlide.cover.core.Logger;
import org.erlide.cover.views.model.ICoverageObject;

/**
 * Generates HTML collective reports
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class ReportGenerator {

    private static ReportGenerator instance;

    private final VelocityEngine ve;
    private final Logger log; // logger

    private ReportGenerator() {
        ve = new VelocityEngine();

        final Properties props = new Properties();

        props.setProperty("resource.loader", "string");
        props.setProperty("string.resource.loader.class",
                "org.apache.velocity.runtime.resource.loader.StringResourceLoader");

        ve.init(props);
        log = Activator.getDefault();
    }

    public static synchronized ReportGenerator getInstance() {
        if (instance == null) {
            instance = new ReportGenerator();
        }
        return instance;
    }

    public String getHTMLreport(final ICoverageObject obj,
            final boolean relative) {
        // organize data

        final String date = Calendar.getInstance().getTime().toString();
        final String type = "file";
        String cssCode = "";
        try {
            final URL bundleRoot = Platform.getBundle(
                    org.erlide.cover.ui.Activator.PLUGIN_ID).getEntry(
                    "/templates/reports.css");
            final BufferedReader stream = new BufferedReader(
                    new InputStreamReader(bundleRoot.openStream()));
            final StringBuilder sb = new StringBuilder();
            String line;
            while ((line = stream.readLine()) != null) {
                sb.append(line).append("\n");
            }

            cssCode = sb.toString();
        } catch (final Exception e) {
            e.printStackTrace();
            log.error(e);
        }

        // add data to a context
        final VelocityContext context = new VelocityContext();
        context.put("obj", obj);
        context.put("children", obj.getChildren());
        context.put("date", date);
        context.put("type", type);
        context.put("css", cssCode);

        try {
            final String templText = getTemplateFromJar(relative);
            StringResourceLoader.getRepository().putStringResource(
                    "my_template", templText);
            final Template t = ve.getTemplate("my_template");

            final StringWriter writer = new StringWriter();
            t.merge(context, writer);

            return writer.toString();
        } catch (final Exception e) {
            e.printStackTrace();
            return null;
        }

    }

    // obtain templates
    private String getTemplateFromJar(final boolean relative)
            throws IOException, URISyntaxException {
        URL bundleRoot;
        if (relative) {
            bundleRoot = Platform.getBundle(
                    org.erlide.cover.ui.Activator.PLUGIN_ID).getEntry(
                    "/templates/reportRel.vm");
        } else {
            bundleRoot = Platform.getBundle(
                    org.erlide.cover.ui.Activator.PLUGIN_ID).getEntry(
                    "/templates/report.vm");
        }

        final BufferedReader stream = new BufferedReader(new InputStreamReader(
                bundleRoot.openStream()));
        final StringBuilder sb = new StringBuilder();
        String line;
        while ((line = stream.readLine()) != null) {
            sb.append(line).append("\n");
        }
        return sb.toString();
    }
}

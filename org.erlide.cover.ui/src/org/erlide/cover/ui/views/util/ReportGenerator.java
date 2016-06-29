package org.erlide.cover.ui.views.util;

import java.util.Calendar;

import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.util.ErlLogger;

/**
 * Generates HTML collective reports
 *
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class ReportGenerator {

    private static ReportGenerator instance;

    private ReportGenerator() {
    }

    public static synchronized ReportGenerator getInstance() {
        if (instance == null) {
            instance = new ReportGenerator();
        }
        return instance;
    }

    public String getHTMLreport(final ICoverageObject obj, final boolean relative) {
        // organize data

        final String date = Calendar.getInstance().getTime().toString();
        final String type = "file";

        try {
            final ReportTemplate template = new ReportTemplate(obj, date, type);
            return template.getReport(relative);
        } catch (final Exception e) {
            ErlLogger.error(e);
            return null;
        }

    }

}

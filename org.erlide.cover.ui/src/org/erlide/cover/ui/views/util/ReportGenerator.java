package org.erlide.cover.ui.views.util;

import java.io.StringWriter;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
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

    private ReportGenerator instance;
    
    private VelocityEngine ve;
    private Logger log;         // logger
    
    private ReportGenerator() {
        ve = new VelocityEngine();
        ve.init();
        log = Activator.getDefault();
;    }
    
    public synchronized ReportGenerator getInstance() {
        if(instance == null)
            instance = new ReportGenerator();
        return instance;
    }
    
    public String getHTMLreport(ICoverageObject obj) {
        //TODO: organize data
        
        //add data to a context
        VelocityContext context = new VelocityContext();
        
        Template t = ve.getTemplate("./src/report.vm");
        
        StringWriter writer = new StringWriter();
        t.merge(context, writer);
        
        return writer.toString();
    }

}

package org.erlide.cover.ui.views.util;

import java.io.StringWriter;
import java.io.Writer;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;

public class VMDemo {

    public static void main(final String[] args) throws Exception {
        Velocity.init();
        final Template t = Velocity.getTemplate("./templates/poc.vm");

        final VelocityContext ctx = new VelocityContext();

        final Writer writer = new StringWriter();
        t.merge(ctx, writer);

        System.out.println(writer);
    }
}

package org.erlide.engine.services.text;

import java.util.Collection;

import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.util.ErlLogger;

public class DocumentationFormatter {

    public static String getDocumentationString(final Collection<IErlComment> comments,
            final IErlTypespec typespec) {
        final StringBuilder stringBuilder = new StringBuilder();
        if (!comments.isEmpty()) {
            stringBuilder.append("<pre class='edoc'>");
            for (final IErlComment member : comments) {
                try {
                    final String source = "\n" + member.getSource();
                    stringBuilder.append(source.replaceAll("\n%%%", "\n")
                            .replaceAll("\n%%", "\n").replaceAll("\n%", "\n").substring(1)
                            .replaceAll("\n( *([-=] *)+\n)+", "\n<hr/>\n")
                            .replaceAll("^ *([-=] *)+\n", "\n")
                            .replaceAll("\n *([-=] *)+$", "\n"));
                    if (!source.endsWith("\n")) {
                        stringBuilder.append('\n');
                    }
                    stringBuilder.append('\n');
                } catch (final ErlModelException e) {
                    ErlLogger.warn(e);
                }
            }
            stringBuilder.append("</pre>");
        }
        if (typespec != null) {
            try {
                stringBuilder.append("<hr/><pre class='typespec'>")
                        .append(typespec.getSource()).append("</pre>");
            } catch (final ErlModelException e) {
                ErlLogger.warn(e);
            }
        }
        return stringBuilder.toString().replace("\n", "<br/>");
    }

}

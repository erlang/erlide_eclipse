package org.erlide.ui.internal.information;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;

import org.eclipse.swt.browser.LocationEvent;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlMember;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.ui.internal.ErlBrowserInformationControlInput;
import org.erlide.ui.util.eclipse.text.HTMLPrinter;
import org.erlide.util.ErlLogger;
import org.erlide.util.ErlangFunctionCall;

public class HoverUtil {

    public static ErlangFunctionCall eventToErlangFunctionCall(
            final String moduleName0, final LocationEvent event) {
        String moduleName = moduleName0;
        final String location = event.location;
        ErlLogger.debug("eventToErlangFunction %s", location);
        final int hashPos = location.lastIndexOf('#');
        if (hashPos > 0) {
            String name = location.substring(hashPos + 1);
            final int colonPos = name.lastIndexOf(':');
            if (colonPos > 0) {
                name = name.substring(colonPos + 1);
            }
            final int slashPos = location.lastIndexOf('/');
            final int dotPos = location.lastIndexOf('.');
            if (slashPos > 0 && dotPos > 0) {
                moduleName = location.substring(slashPos + 1, dotPos);
            }
            final int quotePos = moduleName.lastIndexOf('\'');
            if (quotePos >= 0) {
                moduleName = moduleName.substring(quotePos + 1);
            }
            final int minusPos = name.lastIndexOf('-');
            if (minusPos > 0) {
                final String s = name.substring(minusPos + 1);
                name = name.substring(0, minusPos);
                try {
                    final int i = Integer.parseInt(s);
                    final ErlangFunctionCall erlangFunctionCall = new ErlangFunctionCall(
                            moduleName, name, i);
                    ErlLogger.debug("%s", erlangFunctionCall);
                    return erlangFunctionCall;
                } catch (final NumberFormatException e) {
                }
            }
        }
        return null;
    }

    public static String getHTMLAndReplaceJSLinks(final StringBuffer buffer) {
        String result = buffer.toString();
        if (result.length() > 0) {
            result = result.replace("javascript:erlhref('", "");
            result = result.replace("');\">", "\">");
            result = HTMLPrinter.asHtml(result);
        }
        return result;
    }

    public static URL getBaseDocumentationURL(final IErlElement element) {
        // TODO Auto-generated method stub
        return null;
    }

    public static URL getDocumentationLocation(final IErlElement element) {
        // TODO Auto-generated method stub
        return null;
    }

    public static URL getDocumentationURL(
            final ErlBrowserInformationControlInput input) {
        final String docPath = input.getDocPath();
        final String anchor = input.getAnchor();
        if (docPath != null) {
            try {
                // return new URL("file:" + docPath + "#" + anchor);
                final File file = new File(docPath);
                URL url = file.toURI().toURL();
                if (anchor != null && anchor.length() > 0) {
                    url = new URL(url, "#" + anchor);
                }
                return url;
            } catch (final MalformedURLException e) {
            }
        }
        return null;
    }

    public static String getDocumentationString(
            final Collection<IErlMember> comments) {
        final StringBuilder stringBuilder = new StringBuilder();
        for (final IErlMember member : comments) {
            try {
                final String source = "\n" + member.getSource();
                stringBuilder.append(source.replaceAll("\n%%%", "\n")
                        .replaceAll("\n%%", "\n").replaceAll("\n%", "\n")
                        .substring(1)
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
        return stringBuilder.toString().replace("\n", "<br/>");
    }
}

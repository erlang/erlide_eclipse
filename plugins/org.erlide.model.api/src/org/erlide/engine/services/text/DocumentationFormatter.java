package org.erlide.engine.services.text;

import java.util.Collection;

import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.util.ErlLogger;

public class DocumentationFormatter {

	private DocumentationFormatter() {
	}

	public static String getDocumentationString(final Collection<IErlComment> comments, final IErlTypespec typespec) {
		final StringBuilder stringBuilder = new StringBuilder();
		if (!comments.isEmpty()) {
			stringBuilder.append("<pre class='edoc'>");
			for (final IErlComment member : comments) {
				try {
					String source = "\n" + DocumentationFormatter.convertToHTML(member.getSource());
					source = source.replaceAll("\n%%%", "\n").replaceAll("\n%%", "\n").replaceAll("\n%", "\n").substring(1);
					source = source.replaceAll("\n( *([-=] *)+\n)+", "\n<hr/>\n").replaceAll("^ *([-=] *)+\n", "\n").replaceAll("\n *([-=] *)+$", "\n");
					stringBuilder.append(source);
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
				stringBuilder.append("<hr/><pre class='typespec'>").append(DocumentationFormatter.convertToHTML(typespec.getSource())).append("</pre>");
			} catch (final ErlModelException e) {
				ErlLogger.warn(e);
			}
		}
		return stringBuilder.toString().replace("\n", "<br/>");
	}

	private static Object convertToHTML(String content) {
		String result = content.replaceAll("&", "&amp;");
		result = result.replaceAll("\"", "&quot;");
		result = result.replaceAll("<", "&lt;");
		return result.replaceAll(">", "&gt;");
	}

}

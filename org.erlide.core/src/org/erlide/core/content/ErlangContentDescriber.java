package org.erlide.core.content;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.content.ITextContentDescriber;

import com.google.common.base.Charsets;

public class ErlangContentDescriber implements ITextContentDescriber {
    private static final QualifiedName[] SUPPORTED_OPTIONS = new QualifiedName[] { IContentDescription.CHARSET };
    private static final Pattern LATIN1 = Pattern.compile(
            "%+ +coding: *latin-1", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
    private static final Pattern UTF8 = Pattern.compile(
            "%+ +coding: *UTF-8", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
    private static final String CHARSET = "ErlangContentDescriber.charset"; //$NON-NLS-1$
    private static final String RESULT = "ErlangContentDescriber.processed"; //$NON-NLS-1$

    @Override
    public int describe(final InputStream input,
            final IContentDescription description) throws IOException {
        return describe2(input, description, new HashMap<String, Object>());
    }

    int describe2(final InputStream input,
            final IContentDescription description,
            final Map<String, Object> properties) throws IOException {
        if (!isProcessed(properties)) {
            fillContentProperties(input, description, properties);
        }
        return internalDescribe(description, properties);
    }

    @Override
    public int describe(final Reader input,
            final IContentDescription description) throws IOException {
        return describe2(input, description, new HashMap<String, Object>());
    }

    int describe2(final Reader input, final IContentDescription description,
            final Map<String, Object> properties) throws IOException {
        if (!isProcessed(properties)) {
            fillContentProperties(readEncoding(input), description, properties);
        }
        return internalDescribe(description, properties);
    }

    private boolean isProcessed(final Map<String, Object> properties) {
        final Boolean result = (Boolean) properties.get(RESULT);
        if (result != null) {
            return true;
        }
        return false;
    }

    private void fillContentProperties(final InputStream input,
            final IContentDescription description,
            final Map<String, Object> properties) throws IOException {
        final String encoding = "UTF-8"; //$NON-NLS-1$
        fillContentProperties(readEncoding(input, encoding), description,
                properties);
    }

    private void fillContentProperties(final String charset,
            final IContentDescription description,
            final Map<String, Object> properties) throws IOException {
        if (charset != null) {
            properties.put(CHARSET, charset);
        }
        properties.put(RESULT, new Boolean(true));
    }

    private int internalDescribe(final IContentDescription description,
            final Map<String, Object> properties) {
        if (description == null) {
            return VALID;
        }
        final String charset = (String) properties.get(CHARSET);
        if (description.isRequested(IContentDescription.CHARSET)) {
            if (charset != null && !isCharsetValid(charset)) {
                return INVALID;
            }
            final String charsetName = realCharsetName(charset);
            if (charsetName != null) {
                description.setProperty(IContentDescription.CHARSET,
                        charsetName);
            } else {
                // keep the default setting, as in the
            }
        }
        return VALID;
    }

    private String realCharsetName(final String charset) {
        if ("latin1".equals(charset)) {
            return Charsets.ISO_8859_1.name();
        } else if ("utf8".equals(charset)) {
            return Charsets.UTF_8.name();
        } else {
            return null;
        }
    }

    private boolean isCharsetValid(final String charset) {
        return "latin1".equals(charset) || "utf8".equals(charset);
    }

    private String readEncoding(final InputStream input, final String encoding)
            throws IOException {
        String line = null;

        while ((line = readLine(input)) != null) {
            final String decl = getDeclaration(line);
            if (decl == null) {
                return null;
            }
            if (decl.length() > 0) {
                return decl;
            }
        }
        return null;
    }

    private String readEncoding(final Reader input) throws IOException {
        final BufferedReader reader = new BufferedReader(input);
        String line = null;
        int linesRead = 0;

        while ((line = reader.readLine()) != null) {
            linesRead++;
            if (linesRead > 2) {
                return null;
            }
            final String decl = getDeclaration(line);
            if (decl != null) {
                return decl;
            }
        }
        return null;
    }

    /**
     * @param line
     * @return null if nothing found yet; String if found
     */
    private String getDeclaration(String line) {
        line = line.trim();
        Matcher matcher = LATIN1.matcher(line);
        if (matcher.matches()) {
            return "latin1";
        }
        matcher = UTF8.matcher(line);
        if (matcher.matches()) {
            return "utf8";
        }
        return null;
    }

    @Override
    public QualifiedName[] getSupportedOptions() {
        return SUPPORTED_OPTIONS;
    }

    /**
     * Read a line of data from the underlying InputStream
     * 
     * @return a line stripped of line terminators
     */
    public String readLine(final InputStream in) throws IOException {
        final int _CR = 13;
        final int _LF = 10;
        int _ch = -1; // currently read char

        final StringBuffer sb = new StringBuffer("");
        _ch = in.read();
        while (_ch != _CR && _ch != _LF && _ch != -1) {
            sb.append((char) _ch);
            _ch = in.read();
        }
        if (_ch == -1) {
            return null;
        }
        return (new String(sb));
    }
}

package org.erlide.engine.model.root;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.content.ITextContentDescriber;

import com.google.common.base.Charsets;
import com.google.common.io.Files;

public class ErlangContentDescriber implements ITextContentDescriber {
    private static final QualifiedName[] SUPPORTED_OPTIONS = new QualifiedName[] { IContentDescription.CHARSET };
    private static final Pattern LATIN1 = Pattern.compile(
            "%+[ *-]+coding: *latin-1.*", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
    private static final Pattern UTF8 = Pattern.compile(
            "%+[ *-]+coding: *utf-8.*", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
    private static final String CHARSET = "ErlangContentDescriber.charset"; //$NON-NLS-1$
    private static final String RESULT = "ErlangContentDescriber.processed"; //$NON-NLS-1$

    // private static final Pattern ESCRIPT_TAG =
    // Pattern.compile("#![.]*escript");

    @Override
    public int describe(final InputStream input, final IContentDescription description)
            throws IOException {
        return describe2(input, description, new HashMap<String, Object>());
    }

    int describe2(final InputStream input, final IContentDescription description,
            final Map<String, Object> properties) throws IOException {
        if (!isProcessed(properties)) {
            fillContentProperties(input, description, properties);
        }
        return internalDescribe(description, properties);
    }

    @Override
    public int describe(final Reader input, final IContentDescription description)
            throws IOException {
        return describe2(input, description, new HashMap<String, Object>());
    }

    public static Charset detectEncoding(final String s) {
        if (s == null) {
            return Charsets.ISO_8859_1;
        }
        final String line = s.trim();
        Matcher matcher = LATIN1.matcher(line);
        if (matcher.matches()) {
            return Charsets.ISO_8859_1;
        }
        matcher = UTF8.matcher(line);
        if (matcher.matches()) {
            return Charsets.UTF_8;
        }
        return null;
    }

    public static Charset detectCodingForFile(final File file) throws IOException {
        return detectCodingForFile(file, Charsets.ISO_8859_1);
    }

    public static Charset detectCodingForFile(final File file, final Charset dflt)
            throws IOException {
        final String line = Files.readFirstLine(file, Charsets.ISO_8859_1);
        Charset coding = detectEncoding(line);
        if (coding == null) {
            coding = dflt;
        }
        return coding;
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
            final IContentDescription description, final Map<String, Object> properties)
            throws IOException {
        final String encoding = "UTF-8"; //$NON-NLS-1$
        fillContentProperties(readEncoding(input, encoding), description, properties);
    }

    private void fillContentProperties(final String charset,
            final IContentDescription description, final Map<String, Object> properties) {
        if (charset != null) {
            properties.put(CHARSET, charset);
        }
        properties.put(RESULT, Boolean.TRUE);
    }

    private int internalDescribe(final IContentDescription description,
            final Map<String, Object> properties) {
        if (description == null) {
            return VALID;
        }
        final String charset = (String) properties.get(CHARSET);
        if (description.isRequested(IContentDescription.CHARSET)) {
            if (charset != null) {
                if (!isCharsetValid(charset)) {
                    return INVALID;
                }
                description.setProperty(IContentDescription.CHARSET, charset);
            }
        }
        return VALID;
    }

    private boolean isCharsetValid(final String charset) {
        return Charsets.ISO_8859_1.name().equals(charset)
                || Charsets.UTF_8.name().equals(charset);
    }

    private String readEncoding(final InputStream input, final String encoding)
            throws IOException {
        String line = null;

        while ((line = readLine(input)) != null) {
            final Charset decl = detectEncoding(line);
            if (decl == null) {
                return null;
            }
            return decl.toString();
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
            final Charset decl = detectEncoding(line);
            if (decl != null) {
                return decl.toString();
            }
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
        return new String(sb);
    }

}

package org.erlide.core.content;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.internal.content.Util;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.content.ITextContentDescriber;

import com.google.common.base.Charsets;

public class ErlangContentDescriber implements ITextContentDescriber {
    private static final QualifiedName[] SUPPORTED_OPTIONS = new QualifiedName[] {
            IContentDescription.CHARSET, IContentDescription.BYTE_ORDER_MARK };
    private static final String PREFIX = "-encoding"; //$NON-NLS-1$
    private static final String SUFFIX = "."; //$NON-NLS-1$
    private static final String BOM = "ErlangContentDescriber.bom"; //$NON-NLS-1$
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
        final byte[] bom = Util.getByteOrderMark(input);
        String encoding = "UTF-8"; //$NON-NLS-1$
        input.reset();
        if (bom != null) {
            if (bom == IContentDescription.BOM_UTF_16BE) {
                encoding = "UTF-16BE"; //$NON-NLS-1$
            } else if (bom == IContentDescription.BOM_UTF_16LE) {
                encoding = "UTF-16LE"; //$NON-NLS-1$
            }
            // skip BOM to make comparison simpler
            input.skip(bom.length);
            properties.put(BOM, bom);
        }
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
        if (description != null) {
            final byte[] bom = (byte[]) properties.get(BOM);
            if (bom != null
                    && description
                            .isRequested(IContentDescription.BYTE_ORDER_MARK)) {
                description.setProperty(IContentDescription.BYTE_ORDER_MARK,
                        bom);
            }
        }
        if (description == null) {
            return VALID;
        }
        final String charset = (String) properties.get(CHARSET);
        if (description.isRequested(IContentDescription.CHARSET)) {
            if (charset != null && !isCharsetValid(charset)) {
                return INVALID;
            }
            description.setProperty(IContentDescription.CHARSET,
                    realCharsetName(charset));
        }
        return VALID;
    }

    private String realCharsetName(final String charset) {
        if ("latin1".equals(charset)) {
            return Charsets.ISO_8859_1.name();
        } else if ("utf8".equals(charset)) {
            return Charsets.UTF_8.name();
        } else {
            return Charsets.ISO_8859_1.name();
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

        while ((line = reader.readLine()) != null) {
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

    /**
     * @param line
     * @return null if search should be canceled; "" if nothing found yet;
     *         String if found
     */
    private String getDeclaration(String line) {
        line = line.trim();
        if (line.indexOf(PREFIX) != -1) {
            String decl = line.substring(
                    line.indexOf(PREFIX) + PREFIX.length(),
                    line.indexOf(SUFFIX)).trim();
            if (decl.startsWith("(")) {
                decl = decl.substring(1, decl.length() - 1).trim();
            }
            return decl;
        } else if (!((line.length() == 0) || line.startsWith("-module") || line
                .startsWith("%"))) {
            // cancel search if we find non-attributes
            return null;
        }
        return "";
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

/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.compare;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.compare.IEncodedStreamContentAccessor;
import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlPreprocessorDef;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;

class ErlangCompareUtilities {

    static String getString(final ResourceBundle bundle, final String key,
            final String dfltValue) {

        if (bundle != null) {
            try {
                return bundle.getString(key);
            } catch (final MissingResourceException x) {
                // NeedWork
            }
        }
        return dfltValue;
    }

    static String getString(final ResourceBundle bundle, final String key) {
        return getString(bundle, key, key);
    }

    static int getInteger(final ResourceBundle bundle, final String key,
            final int dfltValue) {

        if (bundle != null) {
            try {
                final String s = bundle.getString(key);
                if (s != null) {
                    return Integer.parseInt(s);
                }
            } catch (final NumberFormatException x) {
                // NeedWork
            } catch (final MissingResourceException x) {
                // NeedWork
            }
        }
        return dfltValue;
    }

    /**
     * Returns a name for the given Erlang element
     */
    static String getErlElementID(final IErlElement e) {
        final StringBuilder sb = new StringBuilder();
        final IErlElement.Kind kind = e.getKind();
        sb.append(kind);
        if (kind == Kind.FUNCTION) {
            final IErlFunction f = (IErlFunction) e;
            sb.append(f.getNameWithArity());
        } else if (kind == Kind.CLAUSE) {
            final IErlFunctionClause fc = (IErlFunctionClause) e;
            sb.append(fc.getHead());
        } else if (kind == Kind.ATTRIBUTE) {
            final IErlAttribute a = (IErlAttribute) e;
            sb.append(a.getName());
            if (a.getValue() != null) {
                sb.append(a.getValue().toString());
            }
        } else if (kind == Kind.RECORD_DEF || kind == Kind.MACRO_DEF) {
            final IErlPreprocessorDef pd = (IErlPreprocessorDef) e;
            sb.append(pd.getDefinedName());
        }

        // xMODULE, xATTRIBUTE, xFUNCTION, xCLAUSE, EXPORT, IMPORT,
        // EXPORTFUNCTION, HEADERCOMMENT, COMMENT, xRECORD_DEF, xMACRO_DEF,
        // FOLDER, TYPESPEC

        return sb.toString();
    }

    /**
     * Reads the contents of the given input stream into a string. The function
     * assumes that the input stream uses the platform's default encoding (
     * <code>ResourcesPlugin.getEncoding()</code>). Returns null if an error
     * occurred.
     */
    private static String readString(final InputStream is, final String encoding) {
        if (is == null) {
            return null;
        }
        BufferedReader reader = null;
        try {
            final StringBuffer buffer = new StringBuffer();
            final char[] part = new char[2048];
            int read = 0;
            reader = new BufferedReader(new InputStreamReader(is, encoding));

            while ((read = reader.read(part)) != -1) {
                buffer.append(part, 0, read);
            }

            return buffer.toString();

        } catch (final IOException ex) {
            // NeedWork
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (final IOException ex) {
                    // silently ignored
                }
            }
        }
        return null;
    }

    public static String readString(final IStreamContentAccessor sa)
            throws CoreException {
        final InputStream is = sa.getContents();
        if (is != null) {
            String encoding = null;
            if (sa instanceof IEncodedStreamContentAccessor) {
                try {
                    encoding = ((IEncodedStreamContentAccessor) sa)
                            .getCharset();
                } catch (final Exception e) {
                }
            }
            if (encoding == null) {
                encoding = ResourcesPlugin.getEncoding();
            }
            try {
                return readString(is, encoding);
            } finally {
                try {
                    is.close();
                } catch (final IOException e) {
                }
            }
        }
        return null;
    }

    /**
     * Returns the contents of the given string as an array of bytes in the
     * platform's default encoding.
     */
    static byte[] getBytes(final String s, final String encoding) {
        try {
            return s.getBytes(encoding);
        } catch (final UnsupportedEncodingException e) {
            return s.getBytes();
        }
    }

    /**
     * Breaks the contents of the given input stream into an array of strings.
     * The function assumes that the input stream uses the platform's default
     * encoding (<code>ResourcesPlugin.getEncoding()</code>). Returns null if an
     * error occurred.
     */
    static String[] readLines(final InputStream is2, final String encoding) {

        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(is2, encoding));
            StringBuffer sb = new StringBuffer();
            final List<String> list = new ArrayList<String>();
            while (true) {
                int c = reader.read();
                if (c == -1) {
                    break;
                }
                sb.append((char) c);
                if (c == '\r') { // single CR or a CR followed by LF
                    c = reader.read();
                    if (c == -1) {
                        break;
                    }
                    sb.append((char) c);
                    if (c == '\n') {
                        list.add(sb.toString());
                        sb = new StringBuffer();
                    }
                } else if (c == '\n') { // a single LF
                    list.add(sb.toString());
                    sb = new StringBuffer();
                }
            }
            if (sb.length() > 0) {
                list.add(sb.toString());
            }
            return list.toArray(new String[list.size()]);

        } catch (final IOException ex) {
            return null;

        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (final IOException ex) {
                    // silently ignored
                }
            }
        }
    }
}

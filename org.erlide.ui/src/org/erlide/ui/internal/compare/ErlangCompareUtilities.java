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
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlPreprocessorDef;

class ErlangCompareUtilities {

    // private static final char PACKAGEDECLARATION = '%';
    // private static final char IMPORTDECLARATION = '#';
    // private static final char IMPORT_CONTAINER = '<';
    // private static final char FIELD = '^';
    // private static final char METHOD = '~';
    // private static final char INITIALIZER = '|';
    // private static final char COMPILATIONUNIT = '{';
    // private static final char TYPE = '[';

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

    // static ImageDescriptor getImageDescriptor(final int type) {
    // switch (type) {
    // case IErlElement.INITIALIZER:
    // case IErlElement.METHOD:
    // return getImageDescriptor("obj16/compare_method.gif"); //$NON-NLS-1$
    // case IErlElement.FIELD:
    // return getImageDescriptor("obj16/compare_field.gif"); //$NON-NLS-1$
    // case IErlElement.PACKAGE_DECLARATION:
    // return JavaPluginImages.DESC_OBJS_PACKDECL;
    // case IErlElement.IMPORT_DECLARATION:
    // return JavaPluginImages.DESC_OBJS_IMPDECL;
    // case IErlElement.IMPORT_CONTAINER:
    // return JavaPluginImages.DESC_OBJS_IMPCONT;
    // case IErlElement.COMPILATION_UNIT:
    // return JavaPluginImages.DESC_OBJS_CUNIT;
    // }
    // return ImageDescriptor.getMissingImageDescriptor();
    // }

    // static ImageDescriptor getTypeImageDescriptor(final boolean isClass) {
    // if (isClass) {
    // return JavaPluginImages.DESC_OBJS_CLASS;
    // }
    // return JavaPluginImages.DESC_OBJS_INTERFACE;
    // }

    // static ImageDescriptor getEnumImageDescriptor() {
    // return JavaPluginImages.DESC_OBJS_ENUM;
    // }

    // static ImageDescriptor getAnnotationImageDescriptor() {
    // return JavaPluginImages.DESC_OBJS_ANNOTATION;
    // }

    // static ImageDescriptor getImageDescriptor(IMember element) {
    // final int t = element.getElementType();
    // if (t == IErlElement.TYPE) {
    // IType type = (IType) element;
    // try {
    // return getTypeImageDescriptor(type.isClass());
    // } catch (final CoreException e) {
    // JavaPlugin.log(e);
    // return JavaPluginImages.DESC_OBJS_GHOST;
    // }
    // }
    // return getImageDescriptor(t);
    // }

    /**
     * Returns a name for the given Java element that uses the same conventions
     * as the JavaNode name of a corresponding element.
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
            sb.append(a.getValue().toString());
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
     * Returns a name which identifies the given typed name. The type is encoded
     * as a single character at the beginning of the string.
     */
    // static String buildID(final int type, final String name) {
    // final StringBuffer sb = new StringBuffer();
    // switch (type) {
    // case JavaNode.CU:
    // sb.append(COMPILATIONUNIT);
    // break;
    // case JavaNode.CLASS:
    // case JavaNode.INTERFACE:
    // case JavaNode.ENUM:
    // case JavaNode.ANNOTATION:
    // sb.append(TYPE);
    // sb.append(name);
    // break;
    // case JavaNode.FIELD:
    // sb.append(FIELD);
    // sb.append(name);
    // break;
    // case JavaNode.CONSTRUCTOR:
    // case JavaNode.METHOD:
    // sb.append(METHOD);
    // sb.append(name);
    // break;
    // case JavaNode.INIT:
    // sb.append(INITIALIZER);
    // sb.append(name);
    // break;
    // case JavaNode.PACKAGE:
    // sb.append(PACKAGEDECLARATION);
    // break;
    // case JavaNode.IMPORT:
    // sb.append(IMPORTDECLARATION);
    // sb.append(name);
    // break;
    // case JavaNode.IMPORT_CONTAINER:
    // sb.append(IMPORT_CONTAINER);
    // break;
    // default:
    // Assert.isTrue(false);
    // break;
    // }
    // return sb.toString();
    // }
    // static ImageDescriptor getImageDescriptor(final String relativePath) {
    // final IPath path = JavaPluginImages.ICONS_PATH.append(relativePath);
    // return JavaPluginImages.createImageDescriptor(JavaPlugin.getDefault()
    // .getBundle(), path, true);
    // }
    // static boolean getBoolean(final CompareConfiguration cc, final String
    // key,
    // final boolean dflt) {
    // if (cc != null) {
    // final Object value = cc.getProperty(key);
    // if (value instanceof Boolean) {
    // return ((Boolean) value).booleanValue();
    // }
    // }
    // return dflt;
    // }
    // static Image getImage(IMember member) {
    // final ImageDescriptor id = getImageDescriptor(member);
    // return id.createImage();
    // }
    // static JavaTextTools getJavaTextTools() {
    // JavaPlugin plugin = JavaPlugin.getDefault();
    // if (plugin != null) {
    // return plugin.getJavaTextTools();
    // }
    // return null;
    // }
    // static IDocumentPartitioner createJavaPartitioner() {
    // JavaTextTools tools = getJavaTextTools();
    // if (tools != null) {
    // return tools.createDocumentPartitioner();
    // }
    // return null;
    // }
    // static void setupDocument(final IDocument document) {
    // JavaTextTools tools = getJavaTextTools();
    // if (tools != null) {
    // tools.setupJavaDocumentPartitioner(document,
    // IJavaPartitions.JAVA_PARTITIONING);
    // }
    // }
    // static void setupPropertiesFileDocument(final IDocument document) {
    // PropertiesFileDocumentSetupParticipant.setupDocument(document);
    // }
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
            return readString(is, encoding);
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

    /*
     * Initialize the given Action from a ResourceBundle.
     */
    // static void initAction(final IAction a, final ResourceBundle bundle,
    // final String prefix) {
    //
    // String labelKey = "label"; //$NON-NLS-1$
    // String tooltipKey = "tooltip"; //$NON-NLS-1$
    // String imageKey = "image"; //$NON-NLS-1$
    // String descriptionKey = "description"; //$NON-NLS-1$
    //
    // if (prefix != null && prefix.length() > 0) {
    // labelKey = prefix + labelKey;
    // tooltipKey = prefix + tooltipKey;
    // imageKey = prefix + imageKey;
    // descriptionKey = prefix + descriptionKey;
    // }
    //
    // a.setText(getString(bundle, labelKey, labelKey));
    // a.setToolTipText(getString(bundle, tooltipKey, null));
    // a.setDescription(getString(bundle, descriptionKey, null));
    //
    // final String relPath = getString(bundle, imageKey, null);
    // if (relPath != null && relPath.trim().length() > 0) {
    //
    // String dPath;
    // String ePath;
    //
    // if (relPath.indexOf("/") >= 0) { //$NON-NLS-1$
    // final String path = relPath.substring(1);
    // dPath = 'd' + path;
    // ePath = 'e' + path;
    // } else {
    // dPath = "dlcl16/" + relPath; //$NON-NLS-1$
    // ePath = "elcl16/" + relPath; //$NON-NLS-1$
    // }
    //
    // ImageDescriptor id = ErlangCompareUtilities.getImageDescriptor(dPath); //
    // we
    // // set
    // // the
    // // disabled
    // // image
    // // first
    // // (see
    // // PR
    // // 1GDDE87)
    // if (id != null) {
    // a.setDisabledImageDescriptor(id);
    // }
    // id = ErlangCompareUtilities.getImageDescriptor(ePath);
    // a.setImageDescriptor(id);
    // if (id != null) {
    // a.setHoverImageDescriptor(id);
    // }
    // }
    // }
    // static void initToggleAction(final IAction a, final ResourceBundle
    // bundle,
    // final String prefix, final boolean checked) {
    //
    // String tooltip = null;
    // if (checked) {
    // tooltip = getString(bundle, prefix + "tooltip.checked", null);
    // //$NON-NLS-1$
    // } else {
    // tooltip = getString(bundle, prefix + "tooltip.unchecked", null);
    // //$NON-NLS-1$
    // }
    // if (tooltip == null) {
    // tooltip = getString(bundle, prefix + "tooltip", null); //$NON-NLS-1$
    // }
    //
    // if (tooltip != null) {
    // a.setToolTipText(tooltip);
    // }
    //
    // String description = null;
    // if (checked) {
    // description = getString(bundle,
    // prefix + "description.checked", null); //$NON-NLS-1$
    // } else {
    // description = getString(bundle,
    // prefix + "description.unchecked", null); //$NON-NLS-1$
    // }
    // if (description == null) {
    // description = getString(bundle, prefix + "description", null);
    // //$NON-NLS-1$
    // }
    //
    // if (description != null) {
    // a.setDescription(description);
    // }
    // }
}

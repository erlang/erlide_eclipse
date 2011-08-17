/*******************************************************************************
 * Copyright (c) 2004 IBM and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.erl.outline;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement;

/**
 * 
 * @version $Revision: 1.10 $
 */
public class ErlangElementLabels {

    /**
     * Method names contain parameter types. e.g. <code>foo(int)</code>
     */
    public static final int M_PARAMETER_TYPES = 1 << 0;

    /**
     * Method names contain parameter names. e.g. <code>foo(index)</code>
     */
    public static final int M_PARAMETER_NAMES = 1 << 1;

    /**
     * Method names contain thrown exceptions. e.g.
     * <code>foo throws IOException</code>
     */
    public static final int M_EXCEPTIONS = 1 << 2;

    /**
     * Method names contain return type (appended) e.g. <code>foo : int</code>
     */
    public static final int M_APP_RETURNTYPE = 1 << 3;

    /**
     * Method names contain return type (appended) e.g. <code>int foo</code>
     */
    public static final int M_PRE_RETURNTYPE = 1 << 4;

    /**
     * Method names are fully qualified. e.g. <code>java.util.Vector.size</code>
     */
    public static final int M_FULLY_QUALIFIED = 1 << 5;

    /**
     * Method names are post qualified. e.g.
     * <code>size - java.util.Vector</code>
     */
    public static final int M_POST_QUALIFIED = 1 << 6;

    /**
     * Initializer names are fully qualified. e.g.
     * <code>java.util.Vector.{ ... }</code>
     */
    public static final int I_FULLY_QUALIFIED = 1 << 7;

    /**
     * Type names are post qualified. e.g. <code>{ ... } - java.util.Map</code>
     */
    public static final int I_POST_QUALIFIED = 1 << 8;

    /**
     * Field names contain the declared type (appended) e.g.
     * <code>fHello : int</code>
     */
    public static final int F_APP_TYPE_SIGNATURE = 1 << 9;

    /**
     * Field names contain the declared type (prepended) e.g.
     * <code>int fHello</code>
     */
    public static final int F_PRE_TYPE_SIGNATURE = 1 << 10;

    /**
     * Fields names are fully qualified. e.g. <code>java.lang.System.out</code>
     */
    public static final int F_FULLY_QUALIFIED = 1 << 11;

    /**
     * Fields names are post qualified. e.g. <code>out - java.lang.System</code>
     */
    public static final int F_POST_QUALIFIED = 1 << 12;

    /**
     * Type names are fully qualified. e.g. <code>java.util.Map.MapEntry</code>
     */
    public static final int T_FULLY_QUALIFIED = 1 << 13;

    /**
     * Type names are type container qualified. e.g. <code>Map.MapEntry</code>
     */
    public static final int T_CONTAINER_QUALIFIED = 1 << 14;

    /**
     * Type names are post qualified. e.g. <code>MapEntry - java.util.Map</code>
     */
    public static final int T_POST_QUALIFIED = 1 << 15;

    /**
     * Declarations (import container / declarartion, package declarartion) are
     * qualified. e.g. <code>java.util.Vector.class/import container</code>
     */
    public static final int D_QUALIFIED = 1 << 16;

    /**
     * Declarations (import container / declarartion, package declarartion) are
     * post qualified. e.g.
     * <code>import container - java.util.Vector.class</code>
     */
    public static final int D_POST_QUALIFIED = 1 << 17;

    /**
     * Class file names are fully qualified. e.g.
     * <code>java.util.Vector.class</code>
     */
    public static final int CF_QUALIFIED = 1 << 18;

    /**
     * Class file names are post qualified. e.g.
     * <code>Vector.class - java.util</code>
     */
    public static final int CF_POST_QUALIFIED = 1 << 19;

    /**
     * Compilation unit names are fully qualified. e.g.
     * <code>java.util.Vector.java</code>
     */
    public static final int CU_QUALIFIED = 1 << 20;

    /**
     * Compilation unit names are post qualified. e.g.
     * <code>Vector.java - java.util</code>
     */
    public static final int CU_POST_QUALIFIED = 1 << 21;

    /**
     * Package names are qualified. e.g. <code>MyProject/src/java.util</code>
     */
    public static final int P_QUALIFIED = 1 << 22;

    /**
     * Package names are post qualified. e.g.
     * <code>java.util - MyProject/src</code>
     */
    public static final int P_POST_QUALIFIED = 1 << 23;

    /**
     * Package Fragment Roots contain variable name if from a variable. e.g.
     * <code>JRE_LIB - c:\java\lib\rt.jar</code>
     */
    public static final int ROOT_VARIABLE = 1 << 24;

    /**
     * Package Fragment Roots contain the project name if not an archive
     * (prepended). e.g. <code>MyProject/src</code>
     */
    public static final int ROOT_QUALIFIED = 1 << 25;

    /**
     * Package Fragment Roots contain the project name if not an archive
     * (appended). e.g. <code>src - MyProject</code>
     */
    public static final int ROOT_POST_QUALIFIED = 1 << 26;

    /**
     * Add root path to all elements except Package Fragment Roots and Erlang
     * projects. e.g. <code>java.lang.Vector - c:\java\lib\rt.jar</code> Option
     * only applies to getElementLabel
     */
    public static final int APPEND_ROOT_PATH = 1 << 27;

    /**
     * Add root path to all elements except Package Fragment Roots and Erlang
     * projects. e.g. <code>java.lang.Vector - c:\java\lib\rt.jar</code> Option
     * only applies to getElementLabel
     */
    public static final int PREPEND_ROOT_PATH = 1 << 28;

    /**
     * Package names are compressed. e.g. <code>o*.e*.search</code>
     */
    public static final int P_COMPRESSED = 1 << 29;

    /**
     * Post qualify referenced package fragement roots. For example
     * <code>jdt.jar - org.eclipse.jdt.ui</code> if the jar is referenced from
     * another project.
     */
    public static final int REFERENCED_ROOT_POST_QUALIFIED = 1 << 30;

    /**
     * Qualify all elements
     */
    public static final int ALL_FULLY_QUALIFIED = F_FULLY_QUALIFIED
            | M_FULLY_QUALIFIED | I_FULLY_QUALIFIED | T_FULLY_QUALIFIED
            | D_QUALIFIED | CF_QUALIFIED | CU_QUALIFIED | P_QUALIFIED
            | ROOT_QUALIFIED;

    /**
     * Post qualify all elements
     */
    public static final int ALL_POST_QUALIFIED = F_POST_QUALIFIED
            | M_POST_QUALIFIED | I_POST_QUALIFIED | T_POST_QUALIFIED
            | D_POST_QUALIFIED | CF_POST_QUALIFIED | CU_POST_QUALIFIED
            | P_POST_QUALIFIED | ROOT_POST_QUALIFIED;

    /**
     * Default options (M_PARAMETER_TYPES enabled)
     */
    public static final int ALL_DEFAULT = M_PARAMETER_TYPES;

    /**
     * Default qualify options (All except Root and Package)
     */
    public static final int DEFAULT_QUALIFIED = F_FULLY_QUALIFIED
            | M_FULLY_QUALIFIED | I_FULLY_QUALIFIED | T_FULLY_QUALIFIED
            | D_QUALIFIED | CF_QUALIFIED | CU_QUALIFIED;

    /**
     * Default post qualify options (All except Root and Package)
     */
    public static final int DEFAULT_POST_QUALIFIED = F_POST_QUALIFIED
            | M_POST_QUALIFIED | I_POST_QUALIFIED | T_POST_QUALIFIED
            | D_POST_QUALIFIED | CF_POST_QUALIFIED | CU_POST_QUALIFIED;

    /**
     * User-readable string for separating post qualified names (e.g. " - ").
     */
    public static final String CONCAT_STRING = " - ";

    /**
     * User-readable string for separating list items (e.g. ", ").
     */
    public static final String COMMA_STRING = ", ";

    /**
     * User-readable string for separating the return type (e.g. " : ").
     */
    public static final String DECL_STRING = ": ";

    /**
     * User-readable string for the default package name (e.g. "(default
     * package)").
     */
    public static final String DEFAULT_PACKAGE = "(default package)";

    private ErlangElementLabels() {
    }

    public static String getTextLabel(final Object obj, final int flags) {
        if (obj instanceof IErlElement) {
            return getElementLabel((IErlElement) obj, flags);
        } else if (obj instanceof IAdaptable) {
            final IWorkbenchAdapter wbadapter = (IWorkbenchAdapter) ((IAdaptable) obj)
                    .getAdapter(IWorkbenchAdapter.class);
            if (wbadapter != null) {
                return wbadapter.getLabel(obj);
            }
        }
        return ""; //$NON-NLS-1$
    }

    /**
     * Returns the label for a Erlang element. Flags as defined above.
     */
    public static String getElementLabel(final IErlElement element,
            final int flags) {
        final StringBuilder buf = new StringBuilder(60);
        getElementLabel(element, flags, buf);
        return buf.toString();
    }

    /**
     * Returns the label for a Erlang element. Flags as defined above.
     */
    public static void getElementLabel(final IErlElement element,
            final int flags, final StringBuilder buf) {
        /*
         * int type= element.getElementType(); IPackageFragmentRoot root= null;
         * 
         * if (type != IErlElement.ERLANG_MODEL && type !=
         * IErlElement.JAVA_PROJECT && type !=
         * IErlElement.PACKAGE_FRAGMENT_ROOT) root=
         * ErlModelUtil.getPackageFragmentRoot(element); if (root != null &&
         * getFlag(flags, PREPEND_ROOT_PATH)) {
         * getPackageFragmentRootLabel(root, ROOT_QUALIFIED, buf);
         * buf.append(CONCAT_STRING); }
         * 
         * switch (type) { case IErlElement.FUNCTION:
         * getFunctionLabel((IErlFunction) element, flags, buf); break; case
         * IErlElement.FIELD: getFieldLabel((IField) element, flags, buf);
         * break; case IErlElement.LOCAL_VARIABLE:
         * getLocalVariableLabel((ILocalVariable) element, flags, buf); break;
         * case IErlElement.INITIALIZER: getInitializerLabel((IInitializer)
         * element, flags, buf); break; case IErlElement.TYPE:
         * getTypeLabel((IType) element, flags, buf); break; case
         * IErlElement.CLASS_FILE: getClassFileLabel((IClassFile) element,
         * flags, buf); break; case IErlElement.COMPILATION_UNIT:
         * getCompilationUnitLabel((ICompilationUnit) element, flags, buf);
         * break; case IErlElement.PACKAGE_FRAGMENT:
         * getPackageFragmentLabel((IPackageFragment) element, flags, buf);
         * break; case IErlElement.PACKAGE_FRAGMENT_ROOT:
         * getPackageFragmentRootLabel((IPackageFragmentRoot) element, flags,
         * buf); break; case IErlElement.IMPORT_CONTAINER: case
         * IErlElement.IMPORT_DECLARATION: case IErlElement.PACKAGE_DECLARATION:
         * getDeclararionLabel(element, flags, buf); break; case
         * IErlElement.JAVA_PROJECT: case IErlElement.JAVA_MODEL:
         * buf.append(element.getElementName()); break; default:
         * buf.append(element.getElementName()); }
         * 
         * if (root != null && getFlag(flags, APPEND_ROOT_PATH)) {
         * buf.append(CONCAT_STRING); getPackageFragmentRootLabel(root,
         * ROOT_QUALIFIED, buf); }
         */}

    /**
     * Appends the label for a method to a StringBuilder. Considers the M_*
     * flags.
     */
    public static void getMethodLabel(final IErlFunction method,
            final int flags, final StringBuilder buf) {
        /*
         * try { // return type if (getFlag(flags, M_PRE_RETURNTYPE) &&
         * method.exists() && !method.isConstructor()) {
         * buf.append(Signature.getSimpleName
         * (Signature.toString(method.getReturnType()))); buf.append(' '); } //
         * qualification if (getFlag(flags, M_FULLY_QUALIFIED)) {
         * getTypeLabel(method.getDeclaringType(), T_FULLY_QUALIFIED | (flags &
         * P_COMPRESSED), buf); buf.append('.'); }
         * 
         * buf.append(method.getElementName()); // parameters buf.append('(');
         * if (getFlag(flags, M_PARAMETER_TYPES | M_PARAMETER_NAMES)) {
         * 
         * 
         * String[] types= getFlag(flags, M_PARAMETER_TYPES) ?
         * method.getParameterTypes() : null; String[] names= (getFlag(flags,
         * M_PARAMETER_NAMES) && method.exists()) ? method.getParameterNames() :
         * null; int nParams= types != null ? types.length : names.length;
         * 
         * for (int i= 0; i < nParams; i++) { if (i > 0) {
         * buf.append(COMMA_STRING); //$NON-NLS-1$ } if (types != null) {
         * buf.append(Signature.getSimpleName(Signature.toString(types[i]))); }
         * if (names != null) { if (types != null) { buf.append(' '); }
         * buf.append(names[i]); } } } else { if
         * (method.getParameterTypes().length > 0) { buf.append("..");
         * //$NON-NLS-1$ } } buf.append(')');
         * 
         * if (getFlag(flags, M_EXCEPTIONS) && method.exists()) { String[]
         * types= method.getExceptionTypes(); if (types.length > 0) {
         * buf.append(" throws "); //$NON-NLS-1$ for (int i= 0; i <
         * types.length; i++) { if (i > 0) { buf.append(COMMA_STRING); }
         * buf.append(Signature.getSimpleName(Signature.toString(types[i]))); }
         * } }
         * 
         * if (getFlag(flags, M_APP_RETURNTYPE) && method.exists() &&
         * !method.isConstructor()) { buf.append(DECL_STRING);
         * buf.append(Signature
         * .getSimpleName(Signature.toString(method.getReturnType()))); } //
         * post qualification if (getFlag(flags, M_POST_QUALIFIED)) {
         * buf.append(CONCAT_STRING); getTypeLabel(method.getDeclaringType(),
         * T_FULLY_QUALIFIED | (flags & P_COMPRESSED), buf); } } catch
         * (ErlModelException e) { ErlPlugin.log(e); // NotExistsException will
         * not reach this point }
         */}

    /**
     * Appends the label for a compilation unit to a StringBuilder. Considers
     * the CU_* flags.
     */
    public static void getCompilationUnitLabel(final IErlModule cu,
            final int flags, final StringBuilder buf) {
        /*
         * if (getFlag(flags, CU_QUALIFIED)) { IPackageFragment pack=
         * (IPackageFragment) cu.getParent(); if (!pack.isDefaultPackage()) {
         * buf.append(pack.getElementName()); buf.append('.'); } }
         * buf.append(cu.getElementName());
         * 
         * if (getFlag(flags, CU_POST_QUALIFIED)) { buf.append(CONCAT_STRING);
         * getPackageFragmentLabel((IPackageFragment) cu.getParent(), 0, buf); }
         */}

    /**
     * Appends the label for a package fragment to a StringBuilder. Considers
     * the P_* flags.
     */
    /*
     * public static void getPackageFragmentLabel(IPackageFragment pack, int
     * flags, StringBuilder buf) { if (getFlag(flags, P_QUALIFIED)) {
     * getPackageFragmentRootLabel((IPackageFragmentRoot) pack.getParent(),
     * ROOT_QUALIFIED, buf); buf.append('/'); } refreshPackageNamePattern(); if
     * (pack.isDefaultPackage()) { buf.append(DEFAULT_PACKAGE); } else if
     * (getFlag(flags, P_COMPRESSED) && fgPkgNameLength >= 0) { String name=
     * pack.getElementName(); int start= 0; int dot= name.indexOf('.', start);
     * while (dot > 0) { if (dot - start > fgPkgNameLength-1) {
     * buf.append(fgPkgNamePrefix); if (fgPkgNameChars > 0)
     * buf.append(name.substring(start, Math.min(start+ fgPkgNameChars, dot)));
     * buf.append(fgPkgNamePostfix); } else buf.append(name.substring(start, dot
     * + 1)); start= dot + 1; dot= name.indexOf('.', start); }
     * buf.append(name.substring(start)); } else {
     * buf.append(pack.getElementName()); } if (getFlag(flags,
     * P_POST_QUALIFIED)) { buf.append(CONCAT_STRING);
     * getPackageFragmentRootLabel((IPackageFragmentRoot) pack.getParent(),
     * ROOT_QUALIFIED, buf); } }
     */
    /**
     * Appends the label for a package fragment root to a StringBuilder.
     * Considers the ROOT_* flags.
     */
    /*
     * public static void getPackageFragmentRootLabel(IPackageFragmentRoot root,
     * int flags, StringBuilder buf) { if (root.isArchive())
     * getArchiveLabel(root, flags, buf); else getFolderLabel(root, flags, buf);
     * }
     */

}

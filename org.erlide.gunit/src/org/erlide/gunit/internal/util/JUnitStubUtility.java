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

package org.erlide.gunit.internal.util;

import org.eclipse.core.runtime.CoreException;

/**
 * Utility methods for code generation. TODO: some methods are duplicated from
 * org.eclipse.jdt.ui
 */
public class JUnitStubUtility {

	public static GenStubSettings getCodeGenerationSettings(IJavaProject project) {
		return new GenStubSettings(project);
	}

	public static class GenStubSettings {
		public boolean callSuper;
		public boolean methodOverwrites;
		public boolean noBody;

		public boolean createComments;
		public boolean useKeywordThis;

		public final int tabWidth;

		public GenStubSettings(IJavaProject project) {
			this.createComments = Boolean.valueOf(
					PreferenceConstants.getPreference(
							PreferenceConstants.CODEGEN_ADD_COMMENTS, project))
					.booleanValue();
			this.useKeywordThis = Boolean.valueOf(
					PreferenceConstants.getPreference(
							PreferenceConstants.CODEGEN_KEYWORD_THIS, project))
					.booleanValue();
			this.tabWidth = IndentManipulation.getTabWidth(project
					.getOptions(true));
		}
	}

	public static String formatCompilationUnit(IJavaProject project,
			String sourceString, String lineDelim) {
		return codeFormat(project, sourceString,
				CodeFormatter.K_COMPILATION_UNIT, 0, lineDelim);
	}

	public static String codeFormat(IJavaProject project, String sourceString,
			int kind, int initialIndentationLevel, String lineDelim) {
		CodeFormatter formatter = ToolFactory.createCodeFormatter(project
				.getOptions(true));
		TextEdit edit = formatter.format(kind, sourceString, 0, sourceString
				.length(), initialIndentationLevel, lineDelim);
		if (edit != null) {
			Document doc = new Document(sourceString);
			try {
				edit.apply(doc);
				return doc.get();
			} catch (MalformedTreeException e) {
			} catch (BadLocationException e) {
			}
		}
		return sourceString;
	}

	/**
	 * Generates a stub. Given a template method, a stub with the same signature
	 * will be constructed so it can be added to a type.
	 * 
	 * @param destTypeName
	 *            The name of the type to which the method will be added to
	 *            (Used for the constructor)
	 * @param method
	 *            A method template (method belongs to different type than the
	 *            parent)
	 * @param settings
	 *            Options as defined above (GENSTUB_*)
	 * @param imports
	 *            Imports required by the sub are added to the imports structure
	 * @return The ynformatted stub
	 * @throws JavaModelException
	 */
	public static String genStub(ICompilationUnit compilationUnit,
			String destTypeName, IMethod method, GenStubSettings settings,
			String extraAnnotations, ImportsManager imports)
			throws CoreException {
		IType declaringtype = method.getDeclaringType();
		StringBuffer buf = new StringBuffer();
		String[] paramTypes = method.getParameterTypes();
		String[] paramNames = method.getParameterNames();
		String[] excTypes = method.getExceptionTypes();
		String retTypeSig = method.getReturnType();

		int lastParam = paramTypes.length - 1;

		String comment = null;
		if (settings.createComments) {
			if (method.isConstructor()) {
				comment = CodeGeneration.getMethodComment(compilationUnit,
						destTypeName, method.getElementName(), paramNames,
						excTypes, null, null, "\n"); //$NON-NLS-1$
			} else {
				if (settings.methodOverwrites) {
					comment = CodeGeneration.getMethodComment(compilationUnit,
							destTypeName, method.getElementName(), paramNames,
							excTypes, retTypeSig, method, "\n"); //$NON-NLS-1$
				} else {
					comment = CodeGeneration.getMethodComment(compilationUnit,
							destTypeName, method.getElementName(), paramNames,
							excTypes, retTypeSig, null, "\n"); //$NON-NLS-1$
				}
			}
		}
		if (comment != null) {
			buf.append(comment).append('\n');
		}
		if (extraAnnotations != null) {
			buf.append(extraAnnotations).append('\n');
		}

		int flags = method.getFlags();
		if (Flags.isPublic(flags)
				|| (declaringtype.isInterface() && !settings.noBody)) {
			buf.append("public "); //$NON-NLS-1$
		} else if (Flags.isProtected(flags)) {
			buf.append("protected "); //$NON-NLS-1$
		} else if (Flags.isPrivate(flags)) {
			buf.append("private "); //$NON-NLS-1$
		}
		if (Flags.isSynchronized(flags)) {
			buf.append("synchronized "); //$NON-NLS-1$
		}
		if (Flags.isVolatile(flags)) {
			buf.append("volatile "); //$NON-NLS-1$
		}
		if (Flags.isStrictfp(flags)) {
			buf.append("strictfp "); //$NON-NLS-1$
		}
		if (Flags.isStatic(flags)) {
			buf.append("static "); //$NON-NLS-1$
		}

		if (method.isConstructor()) {
			buf.append(destTypeName);
		} else {
			String retTypeFrm = Signature.toString(retTypeSig);
			if (!isBuiltInType(retTypeSig)) {
				resolveAndAdd(retTypeSig, declaringtype, imports);
			}
			buf.append(Signature.getSimpleName(retTypeFrm));
			buf.append(' ');
			buf.append(method.getElementName());
		}
		buf.append('(');
		for (int i = 0; i <= lastParam; i++) {
			String paramTypeSig = paramTypes[i];
			String paramTypeFrm = Signature.toString(paramTypeSig);
			if (!isBuiltInType(paramTypeSig)) {
				resolveAndAdd(paramTypeSig, declaringtype, imports);
			}
			buf.append(Signature.getSimpleName(paramTypeFrm));
			buf.append(' ');
			buf.append(paramNames[i]);
			if (i < lastParam) {
				buf.append(", "); //$NON-NLS-1$
			}
		}
		buf.append(')');

		int lastExc = excTypes.length - 1;
		if (lastExc >= 0) {
			buf.append(" throws "); //$NON-NLS-1$
			for (int i = 0; i <= lastExc; i++) {
				String excTypeSig = excTypes[i];
				String excTypeFrm = Signature.toString(excTypeSig);
				resolveAndAdd(excTypeSig, declaringtype, imports);
				buf.append(Signature.getSimpleName(excTypeFrm));
				if (i < lastExc) {
					buf.append(", "); //$NON-NLS-1$
				}
			}
		}
		if (settings.noBody) {
			buf.append(";\n\n"); //$NON-NLS-1$
		} else {
			buf.append(" {\n\t"); //$NON-NLS-1$
			if (!settings.callSuper) {
				if (retTypeSig != null
						&& !retTypeSig.equals(Signature.SIG_VOID)) {
					buf.append('\t');
					if (!isBuiltInType(retTypeSig)
							|| Signature.getArrayCount(retTypeSig) > 0) {
						buf.append("return null;\n\t"); //$NON-NLS-1$
					} else if (retTypeSig.equals(Signature.SIG_BOOLEAN)) {
						buf.append("return false;\n\t"); //$NON-NLS-1$
					} else {
						buf.append("return 0;\n\t"); //$NON-NLS-1$
					}
				}
			} else {
				buf.append('\t');
				if (!method.isConstructor()) {
					if (!Signature.SIG_VOID.equals(retTypeSig)) {
						buf.append("return "); //$NON-NLS-1$
					}
					buf.append("super."); //$NON-NLS-1$
					buf.append(method.getElementName());
				} else {
					buf.append("super"); //$NON-NLS-1$
				}
				buf.append('(');
				for (int i = 0; i <= lastParam; i++) {
					buf.append(paramNames[i]);
					if (i < lastParam) {
						buf.append(", "); //$NON-NLS-1$
					}
				}
				buf.append(");\n\t"); //$NON-NLS-1$
			}
			buf.append("}\n\n"); //$NON-NLS-1$
		}
		return buf.toString();
	}

	private static boolean isBuiltInType(String typeName) {
		char first = Signature.getElementType(typeName).charAt(0);
		return (first != Signature.C_RESOLVED && first != Signature.C_UNRESOLVED);
	}

	private static void resolveAndAdd(String refTypeSig, IType declaringType,
			ImportsManager imports) throws JavaModelException {
		String resolvedTypeName = JavaModelUtil.getResolvedTypeName(refTypeSig,
				declaringType);
		if (resolvedTypeName != null) {
			imports.addImport(resolvedTypeName);
		}
	}

	public static String getTodoTaskTag(IJavaProject project) {
		String markers = null;
		if (project == null) {
			markers = JavaCore.getOption(JavaCore.COMPILER_TASK_TAGS);
		} else {
			markers = project.getOption(JavaCore.COMPILER_TASK_TAGS, true);
		}

		if (markers != null && markers.length() > 0) {
			int idx = markers.indexOf(',');
			if (idx == -1) {
				return markers;
			}
			return markers.substring(0, idx);
		}
		return null;
	}

	/*
	 * Evaluates if a member (possible from another package) is visible from
	 * elements in a package.
	 */
	public static boolean isVisible(IMember member, IPackageFragment pack)
			throws JavaModelException {

		int type = member.getElementType();
		if (type == IJavaElement.INITIALIZER
				|| (type == IJavaElement.METHOD && member.getElementName()
						.startsWith("<"))) { //$NON-NLS-1$
			return false;
		}

		int otherflags = member.getFlags();
		IType declaringType = member.getDeclaringType();
		if (Flags.isPublic(otherflags)
				|| (declaringType != null && declaringType.isInterface())) {
			return true;
		} else if (Flags.isPrivate(otherflags)) {
			return false;
		}

		IPackageFragment otherpack = (IPackageFragment) member
				.getAncestor(IJavaElement.PACKAGE_FRAGMENT);
		return (pack != null && otherpack != null && pack.getElementName()
				.equals(otherpack.getElementName()));
	}

	private static boolean isVersionLessThan(String version1, String version2) {
		return version1.compareTo(version2) < 0;
	}

	public static boolean is50OrHigher(IJavaProject project) {
		return !isVersionLessThan(project.getOption(
				JavaCore.COMPILER_COMPLIANCE, true), JavaCore.VERSION_1_5);
	}

	public static String[] getParameterTypeNamesForSeeTag(IMethod overridden) {
		try {
			ASTParser parser = ASTParser.newParser(AST.JLS3);
			parser.setProject(overridden.getJavaProject());
			IBinding[] bindings = parser.createBindings(
					new IJavaElement[] { overridden }, null);
			if (bindings.length == 1 && bindings[0] instanceof IMethodBinding) {
				return getParameterTypeNamesForSeeTag((IMethodBinding) bindings[0]);
			}
		} catch (IllegalStateException e) {
			// method does not exist
		}
		// fall back code. Not good for generic methods!
		String[] paramTypes = overridden.getParameterTypes();
		String[] paramTypeNames = new String[paramTypes.length];
		for (int i = 0; i < paramTypes.length; i++) {
			paramTypeNames[i] = Signature.toString(Signature
					.getTypeErasure(paramTypes[i]));
		}
		return paramTypeNames;
	}

	private static String[] getParameterTypeNamesForSeeTag(
			IMethodBinding binding) {
		ITypeBinding[] typeBindings = binding.getParameterTypes();
		String[] result = new String[typeBindings.length];
		for (int i = 0; i < result.length; i++) {
			ITypeBinding curr = typeBindings[i];
			if (curr.isTypeVariable()) {
				curr = curr.getErasure(); // in Javadoc only use type variable
											// erasure
			}
			curr = curr.getTypeDeclaration(); // no parameterized types
			result[i] = curr.getQualifiedName();
		}
		return result;
	}

}

/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.gunit.internal.util;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;

import org.erlide.gunit.internal.launcher.ITestKind;
import org.erlide.gunit.internal.launcher.TestKindRegistry;
import org.erlide.gunit.internal.ui.GUnitPlugin;

/**
 * Custom Search engine for suite() methods
 */
public class TestSearchEngine {

	public static boolean isTestOrTestSuite(IType declaringType)
			throws CoreException {
		ITestKind testKind = TestKindRegistry
				.getContainerTestKind(declaringType);
		return testKind.getFinder().isTest(declaringType);
	}

	public static IType[] findTests(IRunnableContext context,
			final IJavaElement element, final ITestKind testKind)
			throws InvocationTargetException, InterruptedException {
		final Set result = new HashSet();

		IRunnableWithProgress runnable = new IRunnableWithProgress() {
			public void run(IProgressMonitor pm) throws InterruptedException,
					InvocationTargetException {
				try {
					testKind.getFinder().findTestsInContainer(element, result,
							pm);
				} catch (CoreException e) {
					throw new InvocationTargetException(e);
				}
			}
		};
		context.run(true, true, runnable);
		return (IType[]) result.toArray(new IType[result.size()]);
	}

	public static boolean isAccessibleClass(IType type)
			throws JavaModelException {
		int flags = type.getFlags();
		if (Flags.isInterface(flags)) {
			return false;
		}
		IJavaElement parent = type.getParent();
		while (true) {
			if (parent instanceof ICompilationUnit
					|| parent instanceof IClassFile) {
				return true;
			}
			if (!(parent instanceof IType) || !Flags.isStatic(flags)
					|| !Flags.isPublic(flags)) {
				return false;
			}
			flags = ((IType) parent).getFlags();
			parent = parent.getParent();
		}
	}

	public static boolean isAccessibleClass(ITypeBinding type)
			throws JavaModelException {
		if (type.isInterface()) {
			return false;
		}
		int modifiers = type.getModifiers();
		while (true) {
			if (type.getDeclaringMethod() != null) {
				return false;
			}
			ITypeBinding declaringClass = type.getDeclaringClass();
			if (declaringClass == null) {
				return true;
			}
			if (!Modifier.isStatic(modifiers) || !Modifier.isPublic(modifiers)) {
				return false;
			}
			modifiers = declaringClass.getModifiers();
			type = declaringClass;
		}
	}

	public static boolean hasTestCaseType(IJavaProject javaProject) {
		try {
			return javaProject != null
					&& javaProject.findType(GUnitPlugin.TEST_SUPERCLASS_NAME) != null;
		} catch (JavaModelException e) {
			// not available
		}
		return false;
	}

	public static boolean hasTestAnnotation(IJavaProject project) {
		try {
			return project != null
					&& project.findType(GUnitPlugin.JUNIT4_ANNOTATION_NAME) != null;
		} catch (JavaModelException e) {
			// not available
		}
		return false;
	}

	public static boolean isTestImplementor(IType type)
			throws JavaModelException {
		ITypeHierarchy typeHier = type.newSupertypeHierarchy(null);
		IType[] superInterfaces = typeHier.getAllInterfaces();
		for (int i = 0; i < superInterfaces.length; i++) {
			if (GUnitPlugin.TEST_INTERFACE_NAME.equals(superInterfaces[i]
					.getFullyQualifiedName('.'))) {
				return true;
			}
		}
		return false;
	}

	public static boolean isTestImplementor(ITypeBinding type) {
		ITypeBinding superType = type.getSuperclass();
		if (superType != null && isTestImplementor(superType)) {
			return true;
		}
		ITypeBinding[] interfaces = type.getInterfaces();
		for (int i = 0; i < interfaces.length; i++) {
			ITypeBinding curr = interfaces[i];
			if (GUnitPlugin.TEST_INTERFACE_NAME.equals(curr.getQualifiedName())
					|| isTestImplementor(curr)) {
				return true;
			}
		}
		return false;
	}

	public static boolean hasSuiteMethod(IType type) throws JavaModelException {
		IMethod method = type.getMethod("suite", new String[0]); //$NON-NLS-1$
		if (!method.exists())
			return false;

		if (!Flags.isStatic(method.getFlags())
				|| !Flags.isPublic(method.getFlags())) {
			return false;
		}
		if (!Signature
				.getSimpleName(Signature.toString(method.getReturnType()))
				.equals(GUnitPlugin.SIMPLE_TEST_INTERFACE_NAME)) {
			return false;
		}
		return true;
	}

	public static IRegion getRegion(IJavaElement element)
			throws JavaModelException {
		IRegion result = JavaCore.newRegion();
		if (element.getElementType() == IJavaElement.JAVA_PROJECT) {
			// for projects only add the contained source folders
			IPackageFragmentRoot[] roots = ((IJavaProject) element)
					.getPackageFragmentRoots();
			for (int i = 0; i < roots.length; i++) {
				if (!roots[i].isArchive()) {
					result.add(roots[i]);
				}
			}
		} else {
			result.add(element);
		}
		return result;
	}

	public static void findTestImplementorClasses(ITypeHierarchy typeHierarchy,
			IType testInterface, IRegion region, Set result)
			throws JavaModelException {
		IType[] subtypes = typeHierarchy.getAllSubtypes(testInterface);
		for (int i = 0; i < subtypes.length; i++) {
			IType type = subtypes[i];
			int cachedFlags = typeHierarchy.getCachedFlags(type);
			if (!Flags.isInterface(cachedFlags)
					&& !Flags.isAbstract(cachedFlags) // do the cheaper tests
														// first
					&& region.contains(type)
					&& TestSearchEngine.isAccessibleClass(type)) {
				result.add(type);
			}
		}
	}

	private static class SuiteMethodTypesCollector extends SearchRequestor {

		private Collection fResult;

		public SuiteMethodTypesCollector(Collection result) {
			fResult = result;
		}

		public void acceptSearchMatch(SearchMatch match) throws CoreException {
			Object enclosingElement = match.getElement();
			if (!(enclosingElement instanceof IMethod))
				return;

			IMethod method = (IMethod) enclosingElement;
			if (!Flags.isStatic(method.getFlags())
					|| !Flags.isPublic(method.getFlags())) {
				return;
			}

			IType declaringType = ((IMethod) enclosingElement)
					.getDeclaringType();
			if (!TestSearchEngine.isAccessibleClass(declaringType)) {
				return;
			}
			fResult.add(declaringType);
		}
	}

	public static void findSuiteMethods(IJavaElement element, Set result,
			IProgressMonitor pm) throws CoreException {
		// fix for bug 36449 JUnit should constrain tests to selected project
		// [JUnit]
		IJavaSearchScope scope = SearchEngine.createJavaSearchScope(
				new IJavaElement[] { element }, IJavaSearchScope.SOURCES);

		SearchRequestor requestor = new SuiteMethodTypesCollector(result);
		int matchRule = SearchPattern.R_EXACT_MATCH
				| SearchPattern.R_CASE_SENSITIVE
				| SearchPattern.R_ERASURE_MATCH;
		SearchPattern suitePattern = SearchPattern
				.createPattern(
						"suite() Test", IJavaSearchConstants.METHOD, IJavaSearchConstants.DECLARATIONS, matchRule); //$NON-NLS-1$
		SearchParticipant[] participants = new SearchParticipant[] { SearchEngine
				.getDefaultSearchParticipant() };
		new SearchEngine().search(suitePattern, participants, scope, requestor,
				pm);
	}

}

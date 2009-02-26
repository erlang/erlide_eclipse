/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Sebastian Davids <sdavids@gmx.de> - bug 48696
 *******************************************************************************/
package org.erlide.gunit.internal.ui;

import java.util.ArrayList;
import java.util.HashSet;

import org.eclipse.core.runtime.CoreException;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.util.JUnitStubUtility;

public class JUnitQuickFixProcessor implements IQuickFixProcessor {

	private static final int JUNIT3 = 1;
	private static final int JUNIT4 = 2;

	private static final HashSet ASSERT_METHOD_NAMES = new HashSet();

	public JUnitQuickFixProcessor() {
		ASSERT_METHOD_NAMES.add("fail"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("assertTrue"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("assertFalse"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("assertEquals"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("assertNotNull"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("assertNull"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("assertSame"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("assertNotSame"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("failNotEquals"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("failSame"); //$NON-NLS-1$
		ASSERT_METHOD_NAMES.add("failNotSame"); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jdt.ui.text.java.IQuickFixProcessor#hasCorrections(org.eclipse
	 * .jdt.core.ICompilationUnit, int)
	 */
	public boolean hasCorrections(ICompilationUnit unit, int problemId) {
		return problemId == IProblem.UndefinedType
				|| problemId == IProblem.ImportNotFound
				|| problemId == IProblem.UndefinedMethod;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jdt.ui.text.java.IQuickFixProcessor#getCorrections(org.eclipse
	 * .jdt.ui.text.java.IInvocationContext,
	 * org.eclipse.jdt.ui.text.java.IProblemLocation[])
	 */
	public IJavaCompletionProposal[] getCorrections(
			final IInvocationContext context, IProblemLocation[] locations) {
		ArrayList res = null;
		for (int i = 0; i < locations.length; i++) {
			IProblemLocation problem = locations[i];
			int id = problem.getProblemId();
			if (IProblem.UndefinedType == id || IProblem.ImportNotFound == id) {
				res = getAddJUnitToBuildPathProposals(context, problem, res);
			} else if (id == IProblem.UndefinedMethod) {
				res = getAddAssertImportProposals(context, problem, res);
			}
		}
		if (res == null || res.isEmpty()) {
			return null;
		}
		return (IJavaCompletionProposal[]) res
				.toArray(new IJavaCompletionProposal[res.size()]);
	}

	private ArrayList getAddAssertImportProposals(IInvocationContext context,
			IProblemLocation problem, ArrayList proposals) {
		String[] args = problem.getProblemArguments();
		if (args.length > 1) {
			String methodName = args[1];
			if (ASSERT_METHOD_NAMES.contains(methodName)
					&& isInsideJUnit4Test(context)) {
				if (proposals == null) {
					proposals = new ArrayList();
				}
				proposals.add(new AddAssertProposal(context.getASTRoot(),
						methodName, 9));
				proposals.add(new AddAssertProposal(context.getASTRoot(),
						"*", 10)); //$NON-NLS-1$
			}
		}
		return proposals;
	}

	private ArrayList getAddJUnitToBuildPathProposals(
			IInvocationContext context, IProblemLocation location,
			ArrayList proposals) {
		try {
			ICompilationUnit unit = context.getCompilationUnit();
			int res = 0;
			String s = unit.getBuffer().getText(location.getOffset(),
					location.getLength());
			if (s.equals("org.junit")) { //$NON-NLS-1$
				res = JUNIT4;
			} else if (s.equals("TestCase") || s.equals("TestSuite") || s.equals("junit")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				res = JUNIT3;
			} else if (s.equals("Test")) { //$NON-NLS-1$
				ASTNode node = location.getCoveredNode(context.getASTRoot());
				if (node != null
						&& node.getLocationInParent() == MarkerAnnotation.TYPE_NAME_PROPERTY) {
					res = JUNIT4;
				} else {
					res = JUNIT3 | JUNIT4;
				}
			} else if (s.equals("RunWith")) { //$NON-NLS-1$
				res = JUNIT4;
			}
			if (res != 0) {
				IJavaProject javaProject = unit.getJavaProject();
				if (JUnitStubUtility.is50OrHigher(javaProject)
						&& ((res & JUNIT4) != 0)) {
					if (proposals == null) {
						proposals = new ArrayList();
					}
					proposals
							.add(new JUnitAddLibraryProposal(true, context, 10));
				}
				if ((res & JUNIT3) != 0) {
					if (proposals == null) {
						proposals = new ArrayList();
					}
					proposals
							.add(new JUnitAddLibraryProposal(false, context, 8));
				}
			}
		} catch (JavaModelException e) {
			GUnitPlugin.log(e.getStatus());
		}
		return proposals;
	}

	private boolean isInsideJUnit4Test(IInvocationContext context) {
		if (!JUnitStubUtility.is50OrHigher(context.getCompilationUnit()
				.getJavaProject())) {
			return false;
		}

		ASTNode node = context.getCoveringNode();
		while (node != null && !(node instanceof BodyDeclaration)) {
			node = node.getParent();
		}
		if (node instanceof MethodDeclaration) {
			IMethodBinding binding = ((MethodDeclaration) node)
					.resolveBinding();
			if (binding != null) {
				IAnnotationBinding[] annotations = binding.getAnnotations();
				for (int i = 0; i < annotations.length; i++) {
					final ITypeBinding annotationType = annotations[i]
							.getAnnotationType();
					if (annotationType != null
							&& GUnitPlugin.JUNIT4_ANNOTATION_NAME
									.equals(annotationType.getQualifiedName()))
						return true;
				}
			}
		}
		return false;
	}

	private static class AddAssertProposal implements IJavaCompletionProposal {

		private final CompilationUnit fAstRoot;
		private final String fMethodName;
		private final int fRelevance;

		public AddAssertProposal(CompilationUnit astRoot, String methodName,
				int relevance) {
			fAstRoot = astRoot;
			fMethodName = methodName;
			fRelevance = relevance;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jdt.ui.text.java.IJavaCompletionProposal#getRelevance()
		 */
		public int getRelevance() {
			return fRelevance;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.text.contentassist.ICompletionProposal#apply(org
		 * .eclipse.jface.text.IDocument)
		 */
		public void apply(IDocument document) {
			try {
				ImportRewrite rewrite = CodeStyleConfiguration
						.createImportRewrite(fAstRoot, true);
				rewrite.addStaticImport("org.junit.Assert", fMethodName, true); //$NON-NLS-1$
				TextEdit edit = rewrite.rewriteImports(null);
				edit.apply(document);
			} catch (MalformedTreeException e) {
			} catch (CoreException e) {
			} catch (BadLocationException e) {
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @seeorg.eclipse.jface.text.contentassist.ICompletionProposal#
		 * getAdditionalProposalInfo()
		 */
		public String getAdditionalProposalInfo() {
			return Messages.format(
					JUnitMessages.JUnitQuickFixProcessor_add_assert_info,
					fMethodName);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @seeorg.eclipse.jface.text.contentassist.ICompletionProposal#
		 * getContextInformation()
		 */
		public IContextInformation getContextInformation() {
			return null;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.text.contentassist.ICompletionProposal#getDisplayString
		 * ()
		 */
		public String getDisplayString() {
			return Messages
					.format(
							JUnitMessages.JUnitQuickFixProcessor_add_assert_description,
							fMethodName);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.text.contentassist.ICompletionProposal#getImage()
		 */
		public Image getImage() {
			return JavaUI.getSharedImages().getImage(
					ISharedImages.IMG_OBJS_IMPDECL);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.text.contentassist.ICompletionProposal#getSelection
		 * (org.eclipse.jface.text.IDocument)
		 */
		public Point getSelection(IDocument document) {
			return null;
		}
	}

}

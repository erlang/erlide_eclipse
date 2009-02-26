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
package org.erlide.gunit.internal.ui;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;

public final class JUnitAddLibraryProposal implements IJavaCompletionProposal {

	private final IInvocationContext fContext;
	private final boolean fIsJunit4;
	private final int fRelevance;

	public JUnitAddLibraryProposal(boolean isJunit4,
			IInvocationContext context, int relevance) {
		fIsJunit4 = isJunit4;
		fContext = context;
		fRelevance = relevance;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jdt.ui.text.java.IJavaCompletionProposal#getRelevance()
	 */
	public int getRelevance() {
		return fRelevance;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.text.contentassist.ICompletionProposal#apply(org.eclipse
	 * .jface.text.IDocument)
	 */
	public void apply(IDocument document) {
		IJavaProject project = fContext.getCompilationUnit().getJavaProject();
		Shell shell = GUnitPlugin.getActiveWorkbenchShell();
		try {
			IClasspathEntry entry = null;
			if (fIsJunit4) {
				entry = BuildPathSupport.getJUnit4ClasspathEntry();
			} else {
				entry = BuildPathSupport.getJUnit3ClasspathEntry();
			}
			if (entry != null) {
				addToClasspath(shell, project, entry,
						new BusyIndicatorRunnableContext());
			}
			// force a reconcile
			int offset = fContext.getSelectionOffset();
			int length = fContext.getSelectionLength();
			String s = document.get(offset, length);
			document.replace(offset, length, s);
		} catch (CoreException e) {
			ErrorDialog.openError(shell,
					JUnitMessages.JUnitAddLibraryProposal_title,
					JUnitMessages.JUnitAddLibraryProposal_cannotAdd, e
							.getStatus());
		} catch (BadLocationException e) {
			// ignore
		}
	}

	private static boolean addToClasspath(Shell shell,
			final IJavaProject project, IClasspathEntry entry,
			IRunnableContext context) throws JavaModelException {
		IClasspathEntry[] oldEntries = project.getRawClasspath();
		ArrayList newEntries = new ArrayList(oldEntries.length + 1);
		boolean added = false;
		for (int i = 0; i < oldEntries.length; i++) {
			IClasspathEntry curr = oldEntries[i];
			if (curr.getEntryKind() == IClasspathEntry.CPE_CONTAINER) {
				IPath path = curr.getPath();
				if (path.equals(entry.getPath())) {
					return true; // already on build path
				} else if (path.matchingFirstSegments(entry.getPath()) > 0) {
					if (!added) {
						curr = entry; // replace
						added = true;
					} else {
						curr = null;
					}
				}
			} else if (curr.getEntryKind() == IClasspathEntry.CPE_VARIABLE) {
				IPath path = curr.getPath();
				if (path.segmentCount() > 0
						&& GUnitPlugin.JUNIT_HOME.equals(path.segment(0))) {
					if (!added) {
						curr = entry; // replace
						added = true;
					} else {
						curr = null;
					}
				}
			}
			if (curr != null) {
				newEntries.add(curr);
			}
		}
		if (!added) {
			newEntries.add(entry);
		}

		final IClasspathEntry[] newCPEntries = (IClasspathEntry[]) newEntries
				.toArray(new IClasspathEntry[newEntries.size()]);
		// fix for 64974 OCE in New JUnit Test Case wizard while workspace is
		// locked [JUnit]
		try {
			context.run(true, false, new IRunnableWithProgress() {
				public void run(IProgressMonitor monitor)
						throws InvocationTargetException, InterruptedException {
					try {
						project.setRawClasspath(newCPEntries, monitor);
					} catch (JavaModelException e) {
						throw new InvocationTargetException(e);
					}
				}
			});
			return true;
		} catch (InvocationTargetException e) {
			Throwable t = e.getTargetException();
			if (t instanceof CoreException) {
				ErrorDialog.openError(shell,
						JUnitMessages.JUnitAddLibraryProposal_title,
						JUnitMessages.JUnitAddLibraryProposal_cannotAdd,
						((CoreException) t).getStatus());
			}
			return false;
		} catch (InterruptedException e) {
			return false;
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.text.contentassist.ICompletionProposal#getSelection
	 * (org.eclipse.jface.text.IDocument)
	 */
	public Point getSelection(IDocument document) {
		return new Point(fContext.getSelectionOffset(), fContext
				.getSelectionLength());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.jface.text.contentassist.ICompletionProposal#
	 * getAdditionalProposalInfo()
	 */
	public String getAdditionalProposalInfo() {
		if (fIsJunit4) {
			return JUnitMessages.JUnitAddLibraryProposal_junit4_info;
		}
		return JUnitMessages.JUnitAddLibraryProposal_info;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.text.contentassist.ICompletionProposal#getDisplayString
	 * ()
	 */
	public String getDisplayString() {
		if (fIsJunit4) {
			return JUnitMessages.JUnitAddLibraryProposa_junit4_label;
		}
		return JUnitMessages.JUnitAddLibraryProposal_label;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getImage()
	 */
	public Image getImage() {
		return JavaUI.getSharedImages()
				.getImage(ISharedImages.IMG_OBJS_LIBRARY);
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
}

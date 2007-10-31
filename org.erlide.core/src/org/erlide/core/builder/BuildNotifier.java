/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.IProblem;

public class BuildNotifier {

	protected IProgressMonitor fMonitor;

	protected boolean fCancelling;

	protected float percentComplete;

	protected float progressPerCompilationUnit;

	protected int fNewErrorCount;

	protected int fFixedErrorCount;

	protected int fNewWarningCount;

	protected int fFixedWarningCount;

	protected int fWorkDone;

	protected int fTotalWork;

	protected String previousSubtask;

	public static int newErrorCount = 0;

	public static int fixedErrorCount = 0;

	public static int newWarningCount = 0;

	public static int fixedWarningCount = 0;

	public static void resetProblemCounters() {
		newErrorCount = 0;
		fixedErrorCount = 0;
		newWarningCount = 0;
		fixedWarningCount = 0;
	}

	public BuildNotifier(IProgressMonitor monitor, IProject project) {
		fMonitor = monitor;
		fCancelling = false;
		fNewErrorCount = newErrorCount;
		fFixedErrorCount = fixedErrorCount;
		fNewWarningCount = newWarningCount;
		fFixedWarningCount = fixedWarningCount;
		fWorkDone = 0;
		fTotalWork = 1000000;
	}

	/**
	 * Notification before a compile that a unit is about to be compiled.
	 */
	public void aboutToCompile(IResource unit) {
		final String message = BuilderMessages.bind(
				BuilderMessages.build_compiling, unit.getFullPath()
						.removeLastSegments(1).makeRelative().toString());
		subTask(message);
	}

	public void begin() {
		if (fMonitor != null) {
			fMonitor.beginTask("", fTotalWork); //$NON-NLS-1$
		}
		previousSubtask = null;
	}

	/**
	 * Check whether the build has been canceled.
	 */
	public void checkCancel() {
		if (fMonitor != null && fMonitor.isCanceled()) {
			throw new OperationCanceledException();
		}
	}

	/**
	 * Check whether the build has been canceled. Must use this call instead of
	 * checkCancel() when within the compiler.
	 */
	public void checkCancelWithinCompiler() {
		if (fMonitor != null && fMonitor.isCanceled() && !fCancelling) {
			// Once the compiler has been canceled, don't check again.
			setCancelling(true);
			//
			// stop compiler
		}
	}

	/**
	 * Notification while within a compile that a unit has finished being
	 * compiled.
	 */
	public void compiled(IResource unit) {
		final String message = BuilderMessages.bind(
				BuilderMessages.build_compiling, unit.getFullPath()
						.removeLastSegments(1).makeRelative().toString());
		subTask(message);
		updateProgressDelta(progressPerCompilationUnit);
		checkCancelWithinCompiler();
	}

	public void done() {
		newErrorCount = fNewErrorCount;
		fixedErrorCount = fFixedErrorCount;
		newWarningCount = fNewWarningCount;
		fixedWarningCount = fFixedWarningCount;

		updateProgress(1.0f);
		subTask(BuilderMessages.build_done);
		if (fMonitor != null) {
			fMonitor.done();
		}
		previousSubtask = null;
	}

	/**
	 * Returns a string describing the problems.
	 */
	protected String problemsMessage() {
		final int numNew = fNewErrorCount + fNewWarningCount;
		final int numFixed = fFixedErrorCount + fFixedWarningCount;
		if (numNew == 0 && numFixed == 0) {
			return ""; //$NON-NLS-1$
		}

		final boolean displayBoth = numNew > 0 && numFixed > 0;
		final StringBuffer buffer = new StringBuffer();
		buffer.append('(');
		if (numNew > 0) {
			// (Found x errors + y warnings)
			buffer.append(BuilderMessages.build_foundHeader);
			buffer.append(' ');
			if (displayBoth || fNewErrorCount > 0) {
				if (fNewErrorCount == 1) {
					buffer.append(BuilderMessages.build_oneError);
				} else {
					buffer.append(BuilderMessages.bind(
							BuilderMessages.build_multipleErrors, String
									.valueOf(fNewErrorCount)));
				}
				if (displayBoth || fNewWarningCount > 0) {
					buffer.append(" + "); //$NON-NLS-1$
				}
			}
			if (displayBoth || fNewWarningCount > 0) {
				if (fNewWarningCount == 1) {
					buffer.append(BuilderMessages.buildOneWarning);
				} else {
					buffer.append(BuilderMessages.bind(
							BuilderMessages.build_multipleWarnings, String
									.valueOf(fNewWarningCount)));
				}
			}
			if (numFixed > 0) {
				buffer.append(", "); //$NON-NLS-1$
			}
		}
		if (numFixed > 0) {
			// (Fixed x errors + y warnings) or (Found x errors + y warnings,
			// Fixed x + y)
			buffer.append(BuilderMessages.build_fixedHeader);
			buffer.append(' ');
			if (displayBoth) {
				buffer.append(String.valueOf(fFixedErrorCount));
				buffer.append(" + "); //$NON-NLS-1$
				buffer.append(String.valueOf(fFixedWarningCount));
			} else {
				if (fFixedErrorCount > 0) {
					if (fFixedErrorCount == 1) {
						buffer.append(BuilderMessages.build_oneError);
					} else {
						buffer.append(BuilderMessages.bind(
								BuilderMessages.build_multipleErrors, String
										.valueOf(fFixedErrorCount)));
					}
					if (fFixedWarningCount > 0) {
						buffer.append(" + "); //$NON-NLS-1$
					}
				}
				if (fFixedWarningCount > 0) {
					if (fFixedWarningCount == 1) {
						buffer.append(BuilderMessages.buildOneWarning);
					} else {
						buffer.append(BuilderMessages.bind(
								BuilderMessages.build_multipleWarnings, String
										.valueOf(fFixedWarningCount)));
					}
				}
			}
		}
		buffer.append(')');
		return buffer.toString();
	}

	/**
	 * Sets the cancelling flag, which indicates we are in the middle of being
	 * cancelled. Certain places (those callable indirectly from the compiler)
	 * should not check cancel again while this is true, to avoid
	 * OperationCanceledException being thrown at an inopportune time.
	 */
	public void setCancelling(boolean cancelling) {
		fCancelling = cancelling;
	}

	/**
	 * Sets the amount of progress to report for compiling each compilation
	 * unit.
	 */
	public void setProgressPerCompilationUnit(float progress) {
		progressPerCompilationUnit = progress;
	}

	public void subTask(String message) {
		final String pm = problemsMessage();
		final String msg = pm.length() == 0 ? message : pm + " " + message; //$NON-NLS-1$

		if (msg.equals(previousSubtask)) {
			return; // avoid refreshing with same one
		}
		// if (JavaBuilder.DEBUG) ErlLogger.debug(msg);
		if (fMonitor != null) {
			fMonitor.subTask(msg);
		}

		previousSubtask = msg;
	}

	protected void updateProblemCounts(IProblem[] newProblems) {
		for (IProblem element : newProblems) {
			if (element.isError()) {
				fNewErrorCount++;
			} else {
				fNewWarningCount++;
			}
		}
	}

	/**
	 * Update the problem counts from one compilation result given the old and
	 * new problems, either of which may be null.
	 */
	protected void updateProblemCounts(IMarker[] oldProblems,
			IProblem[] newProblems) {
		if (newProblems != null) {
			next: for (final IProblem newProblem : newProblems) {
				if (newProblem.getID() == IProblem.Task) {
					continue; // skip task
				}
				final boolean isError = newProblem.isError();
				final String message = newProblem.getMessage();

				if (oldProblems != null) {
					for (int j = 0, m = oldProblems.length; j < m; j++) {
						final IMarker pb = oldProblems[j];
						if (pb == null) {
							continue; // already matched up with a new problem
						}
						final boolean wasError = IMarker.SEVERITY_ERROR == pb
								.getAttribute(IMarker.SEVERITY,
										IMarker.SEVERITY_ERROR);
						if (isError == wasError
								&& message.equals(pb.getAttribute(
										IMarker.MESSAGE, ""))) { //$NON-NLS-1$
							oldProblems[j] = null;
							continue next;
						}
					}
				}
				if (isError) {
					fNewErrorCount++;
				} else {
					fNewWarningCount++;
				}
			}
		}
		if (oldProblems != null) {
			next: for (final IMarker oldProblem : oldProblems) {
				if (oldProblem == null) {
					continue next; // already matched up with a new problem
				}
				final boolean wasError = IMarker.SEVERITY_ERROR == oldProblem
						.getAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
				final String message = oldProblem.getAttribute(IMarker.MESSAGE,
						""); //$NON-NLS-1$

				if (newProblems != null) {
					for (final IProblem pb : newProblems) {
						if (pb.getID() == IProblem.Task) {
							continue; // skip task
						}
						if (wasError == pb.isError()
								&& message.equals(pb.getMessage())) {
							continue next;
						}
					}
				}
				if (wasError) {
					fFixedErrorCount++;
				} else {
					fFixedWarningCount++;
				}
			}
		}
	}

	public void updateProgress(float newPercentComplete) {
		if (newPercentComplete > percentComplete) {
			percentComplete = Math.min(newPercentComplete, 1.0f);
			final int work = Math.round(percentComplete * fTotalWork);
			if (work > fWorkDone) {
				if (fMonitor != null) {
					fMonitor.worked(work - fWorkDone);
				}
				if (ErlangBuilder.isDebugging()) {
					ErlLogger.debug(java.text.NumberFormat.getPercentInstance()
							.format(this.percentComplete));
				}
				fWorkDone = work;
			}
		}
	}

	public void updateProgressDelta(float percentWorked) {
		updateProgress(percentComplete + percentWorked);
	}
}

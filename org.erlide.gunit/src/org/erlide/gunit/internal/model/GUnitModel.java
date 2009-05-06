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

package org.erlide.gunit.internal.model;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchListener;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.erlide.core.erlang.IErlProject;
import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.launcher.GUnitLaunchConfigurationConstants;
import org.erlide.gunit.internal.ui.GUnitPlugin;
import org.erlide.gunit.internal.ui.GUnitPreferencesConstants;
import org.erlide.gunit.internal.ui.TestRunnerViewPart;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Central registry for JUnit test runs.
 */
public final class GUnitModel {

	private final class JUnitLaunchListener implements ILaunchListener {

		/**
		 * Used to track new launches. We need to do this so that we only attach
		 * a TestRunner once to a launch. Once a test runner is connected, it is
		 * removed from the set.
		 */
		private final HashSet<ILaunch> fTrackedLaunches = new HashSet<ILaunch>(20);

		/*
		 * @see ILaunchListener#launchAdded(ILaunch)
		 */
		public void launchAdded(final ILaunch launch) {
			this.fTrackedLaunches.add(launch);
		}

		/*
		 * @see ILaunchListener#launchRemoved(ILaunch)
		 */
		public void launchRemoved(final ILaunch launch) {
			this.fTrackedLaunches.remove(launch);
			// TODO: story for removing old test runs?
			// getDisplay().asyncExec(new Runnable() {
			// public void run() {
			// TestRunnerViewPart testRunnerViewPart=
			// findTestRunnerViewPartInActivePage();
			// if (testRunnerViewPart != null && testRunnerViewPart.isCreated()
			// && launch.equals(testRunnerViewPart.getLastLaunch()))
			// testRunnerViewPart.reset();
			// }
			// });
		}

		/*
		 * @see ILaunchListener#launchChanged(ILaunch)
		 */
		public void launchChanged(final ILaunch launch) {
			if (!this.fTrackedLaunches.contains(launch)) {
				return;
			}

			final ILaunchConfiguration config = launch.getLaunchConfiguration();
			if (config == null) {
				return;
			}

			final IErlProject javaProject = GUnitLaunchConfigurationConstants
			.getErlProject(config);
			if (javaProject == null) {
				return;
			}

			// test whether the launch defines the JUnit attributes
			final String portStr = launch
			.getAttribute(GUnitLaunchConfigurationConstants.ATTR_PORT);
			if (portStr == null) {
				return;
			}
			try {
				final int port = Integer.parseInt(portStr);
				this.fTrackedLaunches.remove(launch);
				getDisplay().asyncExec(new Runnable() {
					public void run() {
						connectTestRunner(launch, javaProject, port);
					}
				});
			} catch (final NumberFormatException e) {
				return;
			}
		}

		private void connectTestRunner(final ILaunch launch, final IErlProject javaProject,
				final int port) {
			showTestRunnerViewPartInActivePage(findTestRunnerViewPartInActivePage());

			// TODO: Do notifications have to be sent in UI thread?
			// Check concurrent access to fTestRunSessions (no problem inside
			// asyncExec())
			final int maxCount = GUnitPlugin.getDefault().getPreferenceStore()
			.getInt(GUnitPreferencesConstants.MAX_TEST_RUNS);
			int toDelete = GUnitModel.this.fTestRunSessions.size() - maxCount;
			while (toDelete > 0) {
				toDelete--;
				final TestRunSession session = GUnitModel.this.fTestRunSessions
				.removeLast();
				notifyTestRunSessionRemoved(session);
			}

			final TestRunSession testRunSession = new TestRunSession(launch,
					javaProject, port);
			addTestRunSession(testRunSession);
		}

		private TestRunnerViewPart showTestRunnerViewPartInActivePage(
				final TestRunnerViewPart testRunner) {
			IWorkbenchPart activePart = null;
			IWorkbenchPage page = null;
			try {
				// TODO: have to force the creation of view part contents
				// otherwise the UI will not be updated
				if (testRunner != null && testRunner.isCreated()) {
					return testRunner;
				}
				page = GUnitPlugin.getActivePage();
				if (page == null) {
					return null;
				}
				activePart = page.getActivePart();
				// show the result view if it isn't shown yet
				return (TestRunnerViewPart) page
				.showView(TestRunnerViewPart.NAME);
			} catch (final PartInitException pie) {
				GUnitPlugin.log(pie);
				return null;
			} finally {
				// restore focus stolen by the creation of the result view
				if (page != null && activePart != null) {
					page.activate(activePart);
				}
			}
		}

		private TestRunnerViewPart findTestRunnerViewPartInActivePage() {
			final IWorkbenchPage page = GUnitPlugin.getActivePage();
			if (page == null) {
				return null;
			}
			return (TestRunnerViewPart) page.findView(TestRunnerViewPart.NAME);
		}

		private Display getDisplay() {
			// Shell shell= getActiveWorkbenchShell();
			// if (shell != null) {
			// return shell.getDisplay();
			// }
			Display display = Display.getCurrent();
			if (display == null) {
				display = Display.getDefault();
			}
			return display;
		}
	}

	private final ListenerList fTestRunSessionListeners = new ListenerList();

	/**
	 * Active test run sessions, youngest first.
	 */
	private final LinkedList/* <TestRunSession> */<TestRunSession> fTestRunSessions = new LinkedList<TestRunSession>();

	private final ILaunchListener fLaunchListener = new JUnitLaunchListener();

	/**
	 * Starts the model (called by the {@link GUnitPlugin} on startup).
	 */
	public void start() {
		final ILaunchManager launchManager = DebugPlugin.getDefault()
		.getLaunchManager();
		launchManager.addLaunchListener(this.fLaunchListener);

		/*
		 * TODO: restore on restart: - only import headers! - only import last n
		 * sessions; remove all other files in historyDirectory
		 */
		// File historyDirectory= GUnitPlugin.getHistoryDirectory();
		// File[] swapFiles= historyDirectory.listFiles();
		// if (swapFiles != null) {
		// Arrays.sort(swapFiles, new Comparator() {
		// public int compare(Object o1, Object o2) {
		// String name1= ((File) o1).getName();
		// String name2= ((File) o2).getName();
		// return name1.compareTo(name2);
		// }
		// });
		// for (int i= 0; i < swapFiles.length; i++) {
		// final File file= swapFiles[i];
		// SafeRunner.run(new ISafeRunnable() {
		// public void run() throws Exception {
		// importTestRunSession(file );
		// }
		// public void handleException(Throwable exception) {
		// GUnitPlugin.log(exception);
		// }
		// });
		// }
		// }
	}

	/**
	 * Stops the model (called by the {@link GUnitPlugin} on shutdown).
	 */
	public void stop() {
		final ILaunchManager launchManager = DebugPlugin.getDefault()
		.getLaunchManager();
		launchManager.removeLaunchListener(this.fLaunchListener);

		final File historyDirectory = GUnitPlugin.getHistoryDirectory();
		final File[] swapFiles = historyDirectory.listFiles();
		if (swapFiles != null) {
			for (int i = 0; i < swapFiles.length; i++) {
				swapFiles[i].delete();
			}
		}

		// for (Iterator iter= fTestRunSessions.iterator(); iter.hasNext();) {
		// final TestRunSession session= (TestRunSession) iter.next();
		// SafeRunner.run(new ISafeRunnable() {
		// public void run() throws Exception {
		// session.swapOut();
		// }
		// public void handleException(Throwable exception) {
		// GUnitPlugin.log(exception);
		// }
		// });
		// }
	}

	public void addTestRunSessionListener(final ITestRunSessionListener listener) {
		this.fTestRunSessionListeners.add(listener);
	}

	public void removeTestRunSessionListener(final ITestRunSessionListener listener) {
		this.fTestRunSessionListeners.remove(listener);
	}

	/**
	 * @return a list of active {@link TestRunSession}s. The list is a copy of
	 *         the internal data structure and modifications do not affect the
	 *         global list of active sessions. The list is sorted by age,
	 *         youngest first.
	 */
	public List<TestRunSession> getTestRunSessions() {
		return new ArrayList<TestRunSession>(this.fTestRunSessions);
	}

	/**
	 * Adds the given {@link TestRunSession} and notifies all registered
	 * {@link ITestRunSessionListener}s.
	 * <p>
	 * <b>To be called in the UI thread only!</b>
	 * </p>
	 * 
	 * @param testRunSession
	 *            the session to add
	 */
	public void addTestRunSession(final TestRunSession testRunSession) {
		Assert.isNotNull(testRunSession);
		Assert.isLegal(!this.fTestRunSessions.contains(testRunSession));
		this.fTestRunSessions.addFirst(testRunSession);
		notifyTestRunSessionAdded(testRunSession);
	}

	/**
	 * Imports a test run session from the given file.
	 * 
	 * @param file
	 *            a file containing a test run session transcript
	 * @return the imported test run session
	 * @throws CoreException
	 *             if the import failed
	 */
	public static TestRunSession importTestRunSession(final File file)
	throws CoreException {
		try {
			final SAXParserFactory parserFactory = SAXParserFactory.newInstance();
			// parserFactory.setValidating(true); // TODO: add DTD and debug
			// flag
			final SAXParser parser = parserFactory.newSAXParser();
			final TestRunHandler handler = new TestRunHandler();
			parser.parse(file, handler);
			final TestRunSession session = handler.getTestRunSession();
			GUnitPlugin.getModel().addTestRunSession(session);
			return session;
		} catch (final ParserConfigurationException e) {
			throwImportError(file, e);
		} catch (final SAXException e) {
			throwImportError(file, e);
		} catch (final IOException e) {
			throwImportError(file, e);
		}
		return null; // does not happen
	}

	public static void importIntoTestRunSession(final File swapFile,
			final TestRunSession testRunSession) throws CoreException {
		try {
			final SAXParserFactory parserFactory = SAXParserFactory.newInstance();
			// parserFactory.setValidating(true); // TODO: add DTD and debug
			// flag
			final SAXParser parser = parserFactory.newSAXParser();
			final TestRunHandler handler = new TestRunHandler(testRunSession);
			parser.parse(swapFile, handler);
		} catch (final ParserConfigurationException e) {
			throwImportError(swapFile, e);
		} catch (final SAXException e) {
			throwImportError(swapFile, e);
		} catch (final IOException e) {
			throwImportError(swapFile, e);
		}
	}

	/**
	 * Exports the given test run session.
	 * 
	 * @param testRunSession
	 *            the test run session
	 * @param file
	 *            the destination
	 * @throws CoreException
	 */
	public static void exportTestRunSession(final TestRunSession testRunSession,
			final File file) throws CoreException {
		FileOutputStream out = null;
		try {
			out = new FileOutputStream(file);
			exportTestRunSession(testRunSession, out);

		} catch (final IOException e) {
			throwExportError(file, e);
		} catch (final TransformerConfigurationException e) {
			throwExportError(file, e);
		} catch (final TransformerException e) {
			throwExportError(file, e);
		} finally {
			if (out != null) {
				try {
					out.close();
				} catch (final IOException e2) {
					GUnitPlugin.log(e2);
				}
			}
		}
	}

	public static void exportTestRunSession(final TestRunSession testRunSession,
			final OutputStream out) throws TransformerFactoryConfigurationError,
			TransformerException {

		final Transformer transformer = TransformerFactory.newInstance()
		.newTransformer();
		final InputSource inputSource = new InputSource();
		final SAXSource source = new SAXSource(new TestRunSessionSerializer(
				testRunSession), inputSource);
		final StreamResult result = new StreamResult(out);
		transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8"); //$NON-NLS-1$
		transformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
		/*
		 * Bug in Xalan: Only indents if proprietary property
		 * org.apache.xalan.templates.OutputProperties.S_KEY_INDENT_AMOUNT is
		 * set.
		 * 
		 * Bug in Xalan as shipped with J2SE 5.0: Does not read the
		 * indent-amount property at all >:-(.
		 */
		try {
			transformer.setOutputProperty(
					"{http://xml.apache.org/xalan/indent-amount", "2"); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (final IllegalArgumentException e) {
			// no indentation today...
		}
		transformer.transform(source, result);
	}

	private static void throwExportError(final File file, final Exception e)
	throws CoreException {
		throw new CoreException(new org.eclipse.core.runtime.Status(
				IStatus.ERROR, GUnitPlugin.getPluginId(), Messages.format(
						ModelMessages.JUnitModel_could_not_write, file
						.getAbsolutePath()), e));
	}

	private static void throwImportError(final File file, final Exception e)
	throws CoreException {
		throw new CoreException(new org.eclipse.core.runtime.Status(
				IStatus.ERROR, GUnitPlugin.getPluginId(), Messages.format(
						ModelMessages.JUnitModel_could_not_read, file
						.getAbsolutePath()), e));
	}

	/**
	 * Removes the given {@link TestRunSession} and notifies all registered
	 * {@link ITestRunSessionListener}s.
	 * <p>
	 * <b>To be called in the UI thread only!</b>
	 * </p>
	 * 
	 * @param testRunSession
	 *            the session to remove
	 */
	public void removeTestRunSession(final TestRunSession testRunSession) {
		final boolean existed = this.fTestRunSessions.remove(testRunSession);
		if (existed) {
			notifyTestRunSessionRemoved(testRunSession);
		}
		testRunSession.removeSwapFile();
	}

	private void notifyTestRunSessionRemoved(final TestRunSession testRunSession) {
		testRunSession.stopTestRun();
		final ILaunch launch = testRunSession.getLaunch();
		if (launch != null) {
			final ILaunchManager launchManager = DebugPlugin.getDefault()
			.getLaunchManager();
			launchManager.removeLaunch(launch);
		}

		final Object[] listeners = this.fTestRunSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestRunSessionListener) listeners[i])
			.sessionRemoved(testRunSession);
		}
	}

	private void notifyTestRunSessionAdded(final TestRunSession testRunSession) {
		final Object[] listeners = this.fTestRunSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestRunSessionListener) listeners[i])
			.sessionAdded(testRunSession);
		}
	}

}

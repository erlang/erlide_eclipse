/*******************************************************************************
 * Copyright (c) 2002, 2004 QNX Software Systems and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     QNX Software Systems - initial API and implementation
 *******************************************************************************/
package org.erlide.erlc.internal.ui.buildconsole;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.text.Assert;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleView;
import org.erlide.core.ErlangPlugin;
import org.erlide.erlc.ErlideErlcPlugin;
import org.erlide.erlc.core.resources.IConsole;
import org.erlide.erlc.internal.ui.preferences.BuildConsolePreferencePage;
import org.erlide.erlc.ui.IBuildConsoleEvent;
import org.erlide.erlc.ui.IBuildConsoleListener;
import org.erlide.erlc.ui.IBuildConsoleManager;

public class BuildConsoleManager implements IBuildConsoleManager,
		IResourceChangeListener, IPropertyChangeListener {

	ListenerList listeners = new ListenerList(1);

	BuildConsole fConsole;

	private final Map<IProject, BuildConsolePartitioner> fConsoleMap = new HashMap<IProject, BuildConsolePartitioner>();

	Color infoColor, outputColor, errorColor;

	BuildConsoleStream infoStream, outputStream, errorStream;

	static public final int BUILD_STREAM_TYPE_INFO = 0;

	static public final int BUILD_STREAM_TYPE_OUTPUT = 1;

	static public final int BUILD_STREAM_TYPE_ERROR = 2;

	private IProject fLastProject;

	public BuildConsoleManager() {
	}

	/**
	 * Notifies the console manager that console activity has started on the
	 * project The manager will open the console if the preference is set to
	 * show the console, and notify listeners
	 */
	protected void startConsoleActivity(IProject project) {
		final Object[] list = listeners.getListeners();
		if (list.length > 0) {
			for (int i = 0; i < list.length; i++) {
				final IBuildConsoleListener listener = (IBuildConsoleListener) list[i];
				final ConsoleEvent event = new ConsoleEvent(
						BuildConsoleManager.this, project,
						IBuildConsoleEvent.CONSOLE_START);
				listener.consoleChange(event);
			}
		}
		showConsole();
	}

	/**
	 * Opens the console view. If the view is already open, it is brought to the
	 * front.
	 */
	protected void showConsole() {
		final IWorkbenchWindow window = ErlideErlcPlugin
				.getActiveWorkbenchWindow();
		if (window != null) {
			final IWorkbenchPage page = window.getActivePage();
			if (page != null) {
				IViewPart consoleView = page
						.findView(IConsoleConstants.ID_CONSOLE_VIEW);
				if (consoleView == null
						&& BuildConsolePreferencePage.isAutoOpenConsole()) {
					final IWorkbenchPart activePart = page.getActivePart();
					try {
						consoleView = page
								.showView(IConsoleConstants.ID_CONSOLE_VIEW);
					} catch (final PartInitException pie) {
						ErlangPlugin.log(pie);
					}
					// restore focus stolen by the creation of the
					// console
					page.activate(activePart);
				} else {
					final boolean bringToTop = shouldBringToTop(consoleView);
					if (bringToTop) {
						page.bringToTop(consoleView);
					}
				}
				if (consoleView instanceof IConsoleView) {
					((IConsoleView) consoleView).display(fConsole);
				}
			}
		}
	}

	boolean shouldBringToTop(IViewPart consoleView) {
		final boolean bringToTop = false;
		if (consoleView instanceof IConsoleView) {
			final IConsoleView cView = (IConsoleView) consoleView;
			return !cView.isPinned()
					&& BuildConsolePreferencePage.isConsoleOnTop();
		}
		return bringToTop;
	}

	/**
	 * Traverses the delta looking for added/removed/changed launch
	 * configuration files.
	 * 
	 * @see IResourceChangeListener#resourceChanged(IResourceChangeEvent)
	 */
	public void resourceChanged(IResourceChangeEvent event) {
		final IResource resource = event.getResource();
		if (resource != null && resource.getType() == IResource.PROJECT) {
			if (event.getType() == IResourceChangeEvent.PRE_DELETE
					|| event.getType() == IResourceChangeEvent.PRE_CLOSE) {
				final IDocumentPartitioner partioner = (IDocumentPartitioner) fConsoleMap
						.remove(resource);
				if (partioner != null) {
					partioner.disconnect();
					final Object[] list = listeners.getListeners();
					if (list.length > 0) {
						for (int i = 0; i < list.length; i++) {
							final IBuildConsoleListener listener = (IBuildConsoleListener) list[i];
							final ConsoleEvent consoleEvent = new ConsoleEvent(
									this, (IProject) resource,
									IBuildConsoleEvent.CONSOLE_CLOSE);
							listener.consoleChange(consoleEvent);
						}
					}
				}
			}
		}
	}

	public void shutdown() {
		if (infoColor != null) {
			infoColor.dispose();
			outputColor.dispose();
			errorColor.dispose();
		}
		ConsolePlugin.getDefault().getConsoleManager().removeConsoles(
				new org.eclipse.ui.console.IConsole[] { fConsole });
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
		ErlideErlcPlugin.getDefault().getPreferenceStore()
				.removePropertyChangeListener(this);
	}

	private void runUI(Runnable run) {
		Display display;
		display = Display.getCurrent();
		if (display == null) {
			display = Display.getDefault();
			display.asyncExec(run);
		} else {
			run.run();
		}
	}

	public void startup() {
		infoStream = new BuildConsoleStream();
		outputStream = new BuildConsoleStream();
		errorStream = new BuildConsoleStream();

		runUI(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			public void run() {
				final Display d = ErlideErlcPlugin.getStandardDisplay();
				// install colors
				fConsole = new BuildConsole(BuildConsoleManager.this);
				ConsolePlugin.getDefault().getConsoleManager().addConsoles(
						new org.eclipse.ui.console.IConsole[] { fConsole });
				infoStream.setConsole(fConsole);
				infoColor = createColor(d,
						BuildConsolePreferencePage.PREF_BUILDCONSOLE_INFO_COLOR);
				infoStream.setColor(infoColor);
				outputStream.setConsole(fConsole);
				outputColor = createColor(
						d,
						BuildConsolePreferencePage.PREF_BUILDCONSOLE_OUTPUT_COLOR);
				outputStream.setColor(outputColor);
				errorStream.setConsole(fConsole);
				errorColor = createColor(
						d,
						BuildConsolePreferencePage.PREF_BUILDCONSOLE_ERROR_COLOR);
				errorStream.setColor(errorColor);
			}
		});
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
		ErlideErlcPlugin.getDefault().getPreferenceStore()
				.addPropertyChangeListener(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent event) {
		final String property = event.getProperty();
		final Display d = ErlideErlcPlugin.getStandardDisplay();
		// colors
		if (property
				.equals(BuildConsolePreferencePage.PREF_BUILDCONSOLE_INFO_COLOR)) {
			final Color newColor = createColor(d,
					BuildConsolePreferencePage.PREF_BUILDCONSOLE_INFO_COLOR);
			infoStream.setColor(newColor);
			infoColor.dispose();
			infoColor = newColor;
		} else if (property
				.equals(BuildConsolePreferencePage.PREF_BUILDCONSOLE_OUTPUT_COLOR)) {
			final Color newColor = createColor(d,
					BuildConsolePreferencePage.PREF_BUILDCONSOLE_OUTPUT_COLOR);
			outputStream.setColor(newColor);
			outputColor.dispose();
			outputColor = newColor;
		} else if (property
				.equals(BuildConsolePreferencePage.PREF_BUILDCONSOLE_ERROR_COLOR)) {
			final Color newColor = createColor(d,
					BuildConsolePreferencePage.PREF_BUILDCONSOLE_ERROR_COLOR);
			errorStream.setColor(newColor);
			errorColor.dispose();
			errorColor = newColor;
		}
	}

	public BuildConsoleStream getStream(int type) throws CoreException {
		switch (type) {
		case BUILD_STREAM_TYPE_ERROR:
			return errorStream;
		case BUILD_STREAM_TYPE_INFO:
			return infoStream;
		case BUILD_STREAM_TYPE_OUTPUT:
			return outputStream;
		}
		throw new CoreException(new Status(IStatus.ERROR,
				ErlideErlcPlugin.PLUGIN_ID, -1, "No Such Console", null)); //$NON-NLS-1$
	}

	/**
	 * Returns a color instance based on data from a preference field.
	 */
	Color createColor(Display display, String preference) {
		final RGB rgb = PreferenceConverter.getColor(ErlideErlcPlugin
				.getDefault().getPreferenceStore(), preference);
		return new Color(display, rgb);
	}

	/**
	 * Returns the console for the project, or <code>null</code> if none.
	 */

	public IConsole getConsole(IProject project) {
		Assert.isNotNull(project);
		fLastProject = project;
		return getConsolePartioner(project).getConsole();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.cdt.ui.IBuildConsoleManager#getLastBuiltProject()
	 */
	public IProject getLastBuiltProject() {
		return fLastProject;
	}

	private BuildConsolePartitioner getConsolePartioner(IProject project) {
		BuildConsolePartitioner partioner = (BuildConsolePartitioner) fConsoleMap
				.get(project);
		if (partioner == null) {
			partioner = new BuildConsolePartitioner(this);
			fConsoleMap.put(project, partioner);
		}
		return partioner;
	}

	/**
	 * Returns the document for the projects console, or <code>null</code> if
	 * none.
	 */
	public IDocument getConsoleDocument(IProject project) {
		Assert.isNotNull(project);
		return getConsolePartioner(project).getDocument();
	}

	public void addConsoleListener(IBuildConsoleListener listener) {
		listeners.add(listener);
	}

	public void removeConsoleListener(IBuildConsoleListener listener) {
		listeners.remove(listener);
	}

}

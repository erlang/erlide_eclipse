/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     David Saff (saff@mit.edu) - bug 102632: [JUnit] Support for JUnit 4.
 *******************************************************************************/
package org.erlide.gunit.servers.launcher;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.gunit.internal.launcher.GUnitLaunchConfigurationConstants;
import org.erlide.gunit.internal.ui.GUnitMessages;
import org.erlide.gunit.internal.ui.GUnitPlugin;

/**
 * The launch shortcut to launch JUnit tests.
 * 
 * <p>
 * This class may be instantiated and subclassed.
 * </p>
 * 
 * @since 3.3
 */
public class GUnitLaunchShortcut implements ILaunchShortcut {

	private static final String EMPTY_STRING = ""; //$NON-NLS-1$

	/**
	 * Default constructor.
	 */
	public GUnitLaunchShortcut() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.ui.IEditorPart,
	 * java.lang.String)
	 */
	public void launch(IEditorPart editor, String mode) {
		IErlElement element = null;
		// FIXME
		// JavaUI.getEditorInputJavaElement(editor .getEditorInput());
		if (element != null) {
			launch(new Object[] { element }, mode);
		} else {
			showNoTestsFoundDialog();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.jface.viewers
	 * .ISelection, java.lang.String)
	 */
	public void launch(ISelection selection, String mode) {
		if (selection instanceof IStructuredSelection) {
			launch(((IStructuredSelection) selection).toArray(), mode);
		} else {
			showNoTestsFoundDialog();
		}
	}

	private void launch(Object[] elements, String mode) {
		// try {
		// IErlElement elementToLaunch = null;
		//
		// if (elements.length == 1) {
		// Object selected = elements[0];
		// if (!(selected instanceof IErlElement)
		// && selected instanceof IAdaptable) {
		// selected = ((IAdaptable) selected)
		// .getAdapter(IErlElement.class);
		// }
		// if (selected instanceof IErlElement) {
		// IErlElement element = (IErlElement) selected;
		// switch (element.getElementType()) {
		// case IErlElement.JAVA_PROJECT:
		// case IErlElement.PACKAGE_FRAGMENT_ROOT:
		// case IErlElement.PACKAGE_FRAGMENT:
		// case IErlElement.TYPE:
		// case IErlElement.METHOD:
		// elementToLaunch = element;
		// break;
		// case IErlElement.CLASS_FILE:
		// elementToLaunch = ((IClassFile) element).getType();
		// break;
		// case IErlElement.COMPILATION_UNIT:
		// elementToLaunch = findTypeToLaunch(
		// (ICompilationUnit) element, mode);
		// break;
		// }
		// }
		// }
		// if (elementToLaunch == null) {
		// showNoTestsFoundDialog();
		// return;
		// }
		// performLaunch(elementToLaunch, mode);
		// } catch (InterruptedException e) {
		// // OK, silently move on
		// } catch (CoreException e) {
		// ExceptionHandler.handle(e, getShell(),
		// JUnitMessages.JUnitLaunchShortcut_dialog_title,
		// JUnitMessages.JUnitLaunchShortcut_message_launchfailed);
		// } catch (InvocationTargetException e) {
		// ExceptionHandler.handle(e, getShell(),
		// JUnitMessages.JUnitLaunchShortcut_dialog_title,
		// JUnitMessages.JUnitLaunchShortcut_message_launchfailed);
		// }
	}

	private void showNoTestsFoundDialog() {
		MessageDialog.openInformation(getShell(),
				GUnitMessages.JUnitLaunchShortcut_dialog_title,
				GUnitMessages.JUnitLaunchShortcut_message_notests);
	}

	// private IErlModule findTypeToLaunch(ICompilationUnit cu, String mode)
	// throws CoreException, InterruptedException,
	// InvocationTargetException {
	// ITestKind testKind = TestKindRegistry.getContainerTestKind(cu);
	// IErlModule[] types = TestSearchEngine.findTests(PlatformUI.getWorkbench()
	// .getActiveWorkbenchWindow(), cu, testKind);
	// if (types.length == 0) {
	// return null;
	// } else if (types.length > 1) {
	// return chooseType(types, mode);
	// }
	// return types[0];
	// }
	//
	// private void performLaunch(IErlElement element, String mode)
	// throws InterruptedException, CoreException {
	// ILaunchConfigurationWorkingCopy temparary =
	// createLaunchConfiguration(element);
	// ILaunchConfiguration config = findExistingLaunchConfiguration(
	// temparary, mode);
	// if (config == null) {
	// // no existing found: create a new one
	// config = temparary.doSave();
	// }
	// DebugUITools.launch(config, mode);
	// }
	//
	// private IErlModule chooseType(IErlModule[] types, String mode)
	// throws InterruptedException {
	// ElementListSelectionDialog dialog = new ElementListSelectionDialog(
	// getShell(), new JavaElementLabelProvider(
	// JavaElementLabelProvider.SHOW_POST_QUALIFIED));
	// dialog.setElements(types);
	// dialog.setTitle(JUnitMessages.JUnitLaunchShortcut_dialog_title2);
	// if (mode.equals(ILaunchManager.DEBUG_MODE)) {
	// dialog
	// .setMessage(JUnitMessages.JUnitLaunchShortcut_message_selectTestToDebug);
	// } else {
	// dialog
	// .setMessage(JUnitMessages.JUnitLaunchShortcut_message_selectTestToRun);
	// }
	// dialog.setMultipleSelection(false);
	// if (dialog.open() == Window.OK) {
	// return (IErlModule) dialog.getFirstResult();
	// }
	// throw new InterruptedException(); // cancelled by user
	// }

	private Shell getShell() {
		return GUnitPlugin.getActiveWorkbenchShell();
	}

	private ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}

	/**
	 * Show a selection dialog that allows the user to choose one of the
	 * specified launch configurations. Return the chosen config, or
	 * <code>null</code> if the user cancelled the dialog.
	 * 
	 * @param configList
	 * @param mode
	 * @return ILaunchConfiguration
	 * @throws InterruptedException
	 */
	private ILaunchConfiguration chooseConfiguration(
			List<ILaunchConfiguration> configList, String mode)
			throws InterruptedException {
		IDebugModelPresentation labelProvider = DebugUITools
				.newDebugModelPresentation();
		ElementListSelectionDialog dialog = new ElementListSelectionDialog(
				getShell(), labelProvider);
		dialog.setElements(configList.toArray());
		dialog
				.setTitle(GUnitMessages.JUnitLaunchShortcut_message_selectConfiguration);
		if (mode.equals(ILaunchManager.DEBUG_MODE)) {
			dialog
					.setMessage(GUnitMessages.JUnitLaunchShortcut_message_selectDebugConfiguration);
		} else {
			dialog
					.setMessage(GUnitMessages.JUnitLaunchShortcut_message_selectRunConfiguration);
		}
		dialog.setMultipleSelection(false);
		int result = dialog.open();
		if (result == Window.OK) {
			return (ILaunchConfiguration) dialog.getFirstResult();
		}
		throw new InterruptedException(); // cancelled by user
	}

	/**
	 * Returns the launch configuration type id of the launch configuration this
	 * shortcut will create. Clients can override this method to return the id
	 * of their launch configuration.
	 * 
	 * @return the launch configuration type id of the launch configuration this
	 *         shortcut will create
	 */
	protected String getLaunchConfigurationTypeId() {
		return GUnitLaunchConfigurationConstants.ID_JUNIT_APPLICATION;
	}

	/**
	 * Creates a launch configuration working copy for the given element. The
	 * launch configuration type created will be of the type returned by
	 * {@link #getLaunchConfigurationTypeId}. The element type can only be of
	 * type {@link IErlProject}, {@link IPackageFragmentRoot},
	 * {@link IPackageFragment}, {@link IErlModule} or {@link IMethod}.
	 * 
	 * Clients can extend this method (should call super) to configure
	 * additional attributes on the launch configuration working copy.
	 * 
	 * @return a launch configuration working copy for the given element
	 */
	protected ILaunchConfigurationWorkingCopy createLaunchConfiguration(
			IErlElement element) throws CoreException {
		final String testName;
		// final String mainTypeQualifiedName;
		// final String containerHandleId;
		//
		// switch (element.getKind()) {
		// case PROJECT: {
		// String name = ErlElementLabels.getTextLabel(element,
		// JavaElementLabels.ALL_FULLY_QUALIFIED);
		// containerHandleId = element.getHandleIdentifier();
		// mainTypeQualifiedName = EMPTY_STRING;
		// testName = name.substring(name.lastIndexOf(IPath.SEPARATOR) + 1);
		// }
		// break;
		// case IErlElement.TYPE: {
		// containerHandleId = EMPTY_STRING;
		// mainTypeQualifiedName = JavaModelUtil
		// .getFullyQualifiedName((IErlModule) element); // don't replace,
		// // fix for
		// // binary inner
		// // types
		// testName = element.getElementName();
		// }
		// break;
		// case IErlElement.METHOD: {
		// IMethod method = (IMethod) element;
		// containerHandleId = EMPTY_STRING;
		// mainTypeQualifiedName = JavaModelUtil.getFullyQualifiedName(method
		// .getDeclaringType());
		// testName = method.getDeclaringType().getElementName() + '.'
		// + method.getElementName();
		// }
		// break;
		// default:
		// throw new IllegalArgumentException(
		//					"Invalid element type to create a launch configuration: " + element.getClass().getName()); //$NON-NLS-1$
		// }
		//
		// String testKindId = TestKindRegistry.getContainerTestKindId(element);
		//
		ILaunchConfigurationType configType = getLaunchManager()
				.getLaunchConfigurationType(getLaunchConfigurationTypeId());
		testName = element.getName();
		ILaunchConfigurationWorkingCopy wc = configType.newInstance(null,
				getLaunchManager().generateUniqueLaunchConfigurationNameFrom(
						testName));

		// FIXME
		// wc.setAttribute(IErlLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
		// mainTypeQualifiedName);
		// wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
		// element.getJavaProject().getElementName());
		wc.setAttribute(GUnitLaunchConfigurationConstants.ATTR_KEEPRUNNING,
				false);
		// wc.setAttribute(GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
		// containerHandleId);
		// wc.setAttribute(
		// GUnitLaunchConfigurationConstants.ATTR_TEST_RUNNER_KIND,
		// testKindId);
		if (element instanceof IErlFunction) {
			wc.setAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
					element.getName()); // only set for methods
		}
		return wc;
	}

	/**
	 * Returns the attribute names of the attributes that are compared when
	 * looking for an existing similar launch configuration. Clients can
	 * override and replace to customize.
	 * 
	 * @return the attribute names of the attributes that are compared
	 */
	protected String[] getAttributeNamesToCompare() {
		return new String[] {
		// IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
		// GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
		// IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
		// GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME
		};
	}

	private static boolean hasSameAttributes(ILaunchConfiguration config1,
			ILaunchConfiguration config2, String[] attributeToCompare) {
		try {
			for (int i = 0; i < attributeToCompare.length; i++) {
				String val1 = config1.getAttribute(attributeToCompare[i],
						EMPTY_STRING);
				String val2 = config2.getAttribute(attributeToCompare[i],
						EMPTY_STRING);
				if (!val1.equals(val2)) {
					return false;
				}
			}
			return true;
		} catch (CoreException e) {
			// ignore access problems here, return false
		}
		return false;
	}

	private ILaunchConfiguration findExistingLaunchConfiguration(
			ILaunchConfigurationWorkingCopy temporary, String mode)
			throws InterruptedException, CoreException {
		ILaunchConfigurationType configType = temporary.getType();

		ILaunchConfiguration[] configs = getLaunchManager()
				.getLaunchConfigurations(configType);
		String[] attributeToCompare = getAttributeNamesToCompare();

		ArrayList<ILaunchConfiguration> candidateConfigs = new ArrayList<ILaunchConfiguration>(
				configs.length);
		for (int i = 0; i < configs.length; i++) {
			ILaunchConfiguration config = configs[i];
			if (hasSameAttributes(config, temporary, attributeToCompare)) {
				candidateConfigs.add(config);
			}
		}

		// If there are no existing configs associated with the IErlModule,
		// create
		// one.
		// If there is exactly one config associated with the IErlModule, return
		// it.
		// Otherwise, if there is more than one config associated with the
		// IErlModule, prompt the
		// user to choose one.
		int candidateCount = candidateConfigs.size();
		if (candidateCount == 0) {
			return null;
		} else if (candidateCount == 1) {
			return candidateConfigs.get(0);
		} else {
			// Prompt the user to choose a config. A null result means the user
			// cancelled the dialog, in which case this method returns null,
			// since cancelling the dialog should also cancel launching
			// anything.
			ILaunchConfiguration config = chooseConfiguration(candidateConfigs,
					mode);
			if (config != null) {
				return config;
			}
		}
		return null;
	}
}

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
package org.erlide.gunit.launcher;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.variables.VariablesPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModule;
import org.erlide.gunit.internal.launcher.GUnitLaunchConfigurationConstants;
import org.erlide.gunit.internal.ui.GUnitMessages;
import org.erlide.gunit.internal.ui.GUnitPlugin;
import org.erlide.gunit.servers.launcher.ITestKind;
import org.erlide.runtime.backend.ErlangLaunchConfigurationDelegate;

/**
 * Launch configuration delegate for a JUnit test as a Java application.
 * 
 * <p>
 * Clients can instantiate and extend this class.
 * </p>
 * 
 * @since 3.3
 */
public class GUnitLaunchConfigurationDelegate extends
		ErlangLaunchConfigurationDelegate {

	private boolean fKeepAlive = false;

	private int fPort;

	private IErlMember[] fTestElements;

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.debug.core.model.ILaunchConfigurationDelegate#launch(org.
	 * eclipse.debug.core.ILaunchConfiguration, java.lang.String,
	 * org.eclipse.debug.core.ILaunch,
	 * org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}

		monitor.beginTask(MessageFormat.format(
				"{0}...", configuration.getName()), 5); //$NON-NLS-1$
		// check for cancellation
		if (monitor.isCanceled()) {
			return;
		}

		try {
			if (mode
					.equals(GUnitLaunchConfigurationConstants.MODE_RUN_QUIETLY_MODE)) {
				launch.setAttribute(
						GUnitLaunchConfigurationConstants.ATTR_NO_DISPLAY,
						"true"); //$NON-NLS-1$
				mode = ILaunchManager.RUN_MODE;
			}

			monitor
					.subTask(GUnitMessages.JUnitLaunchConfigurationDelegate_verifying_attriburtes_description);

			try {
				preLaunchCheck(configuration, launch, new SubProgressMonitor(
						monitor, 2));
			} catch (CoreException e) {
				if (e.getStatus().getSeverity() == IStatus.CANCEL) {
					monitor.setCanceled(true);
					return;
				}
				throw e;
			}
			// check for cancellation
			if (monitor.isCanceled()) {
				return;
			}

			this.fKeepAlive = mode.equals(ILaunchManager.DEBUG_MODE)
					&& configuration.getAttribute(
							GUnitLaunchConfigurationConstants.ATTR_KEEPRUNNING,
							false);
			this.fPort = evaluatePort();
			launch.setAttribute(GUnitLaunchConfigurationConstants.ATTR_PORT,
					String.valueOf(this.fPort));

			this.fTestElements = evaluateTests(configuration,
					new SubProgressMonitor(monitor, 1));

			String mainTypeName = verifyMainTypeName(configuration);
			// IVMRunner runner = getVMRunner(configuration, mode);
			//
			// File workingDir = verifyWorkingDirectory(configuration);
			// String workingDirName = null;
			// if (workingDir != null) {
			// workingDirName = workingDir.getAbsolutePath();
			// }
			//
			// // Environment variables
			// String[] envp = getEnvironment(configuration);
			//
			// ArrayList vmArguments = new ArrayList();
			// ArrayList programArguments = new ArrayList();
			// collectExecutionArguments(configuration, vmArguments,
			// programArguments);
			//
			// // VM-specific attributes
			// Map vmAttributesMap = getVMSpecificAttributesMap(configuration);
			//
			// // Classpath
			// String[] classpath = getClasspath(configuration);
			//
			// // Create VM config
			// VMRunnerConfiguration runConfig = new VMRunnerConfiguration(
			// mainTypeName, classpath);
			// runConfig.setVMArguments((String[]) vmArguments
			// .toArray(new String[vmArguments.size()]));
			// runConfig.setProgramArguments((String[]) programArguments
			// .toArray(new String[programArguments.size()]));
			// runConfig.setEnvironment(envp);
			// runConfig.setWorkingDirectory(workingDirName);
			// runConfig.setVMSpecificAttributesMap(vmAttributesMap);
			//
			// // Bootpath
			// runConfig.setBootClassPath(getBootpath(configuration));
			//
			// // check for cancellation
			// if (monitor.isCanceled()) {
			// return;
			// }
			//
			// // done the verification phase
			// monitor.worked(1);
			//
			// monitor
			// .subTask(JUnitMessages.JUnitLaunchConfigurationDelegate_create_source_locator_description);
			// // set the default source locator if required
			// setDefaultSourceLocator(launch, configuration);
			// monitor.worked(1);

			// // Launch the configuration - 1 unit of work
			// runner.run(runConfig, launch, monitor);

			// check for cancellation
			if (monitor.isCanceled()) {
				return;
			}
		} finally {
			this.fTestElements = null;
			monitor.done();
		}
	}

	private int evaluatePort() throws CoreException {
		// int port = SocketUtil.findFreePort();
		// if (port == -1) {
		// informAndAbort(
		// JUnitMessages.JUnitLaunchConfigurationDelegate_error_no_socket,
		// null,
		// IJavaLaunchConfigurationConstants.ERR_NO_SOCKET_AVAILABLE);
		// }
		// return port;
		return -1;
	}

	/**
	 * Performs a check on the launch configuration's attributes. If an
	 * attribute contains an invalid value, a {@link CoreException} with the
	 * error is thrown.
	 * 
	 * @param configuration
	 *            the launch configuration to verify
	 * @param launch
	 *            the launch to verify
	 * @param monitor
	 *            the progress monitor to use
	 * @throws CoreException
	 *             an exception is thrown when the verification fails
	 */
	protected void preLaunchCheck(ILaunchConfiguration configuration,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		// try {
		// IErlProject javaProject = getErlProject(configuration);
		// if ((javaProject == null) || !javaProject.exists()) {
		// informAndAbort(
		// JUnitMessages.JUnitLaunchConfigurationDelegate_error_invalidproject,
		// null,
		// IJavaLaunchConfigurationConstants.ERR_NOT_A_JAVA_PROJECT);
		// }
		// if (!TestSearchEngine.hasTestCaseType(javaProject)) {
		// informAndAbort(
		// JUnitMessages.JUnitLaunchConfigurationDelegate_error_junitnotonpath,
		// null, IJUnitStatusConstants.ERR_JUNIT_NOT_ON_PATH);
		// }
		//
		// ITestKind testKind = getTestRunnerKind(configuration);
		// boolean isJUnit4Configuration = TestKindRegistry.JUNIT4_TEST_KIND_ID
		// .equals(testKind.getId());
		// if (isJUnit4Configuration
		// && !TestSearchEngine.hasTestAnnotation(javaProject)) {
		// informAndAbort(
		// JUnitMessages.JUnitLaunchConfigurationDelegate_error_junit4notonpath,
		// null, IJUnitStatusConstants.ERR_JUNIT_NOT_ON_PATH);
		// }
		// } finally {
		// monitor.done();
		// }
	}

	private ITestKind getTestRunnerKind(ILaunchConfiguration configuration) {
		ITestKind testKind = GUnitLaunchConfigurationConstants
				.getTestRunnerKind(configuration);
		// if (testKind.isNull()) {
		// testKind = TestKindRegistry.getDefault().getKind(
		// TestKindRegistry.JUNIT3_TEST_KIND_ID); // backward
		// // compatible for
		// // launch
		// // configurations
		// // with no runner
		// }
		return testKind;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate#
	 * verifyMainTypeName(org.eclipse.debug.core.ILaunchConfiguration)
	 */
	public String verifyMainTypeName(ILaunchConfiguration configuration)
			throws CoreException {
		return "org.erlide.gunit.internal.runner.RemoteTestRunner"; //$NON-NLS-1$
	}

	/**
	 * Evaluates all test elements selected by the given launch configuration.
	 * The elements are of type {@link IErlModule} or {@link IMethod}. At the
	 * moment it is only possible to run a single method or a set of types, but
	 * not mixed or more than one method at a time.
	 * 
	 * @param configuration
	 *            the launch configuration to inspect
	 * @param monitor
	 *            the progress monitor
	 * @return returns all types or methods that should be ran
	 * @throws CoreException
	 *             an exception is thrown when the search for tests failed
	 */
	protected IErlMember[] evaluateTests(ILaunchConfiguration configuration,
			IProgressMonitor monitor) throws CoreException {
		// IErlProject javaProject = getErlProject(configuration);
		//
		// IErlElement testTarget = getTestTarget(configuration, javaProject);
		// String testMethodName = configuration.getAttribute(
		//				GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME, ""); //$NON-NLS-1$
		// if (testMethodName.length() > 0) {
		// if (testTarget instanceof IErlModule) {
		// return new IMember[] { ((IErlModule) testTarget).getMethod(
		// testMethodName, new String[0]) };
		// }
		// }
		// HashSet result = new HashSet();
		// ITestKind testKind = getTestRunnerKind(configuration);
		// testKind.getFinder().findTestsInContainer(testTarget, result,
		// monitor);
		// if (result.isEmpty()) {
		// String msg = Messages
		// .format(
		// JUnitMessages.JUnitLaunchConfigurationDelegate_error_notests_kind,
		// testKind.getDisplayName());
		// informAndAbort(msg, null,
		// IJavaLaunchConfigurationConstants.ERR_UNSPECIFIED_MAIN_TYPE);
		// }
		// return (IMember[]) result.toArray(new IMember[result.size()]);
		return null;
	}

	private void informAndAbort(String message, Throwable exception, int code)
			throws CoreException {
		IStatus status = new Status(IStatus.INFO, GUnitPlugin.PLUGIN_ID, code,
				message, exception);
		if (showStatusMessage(status)) {
			// Status message successfully shown
			// -> Abort with INFO exception
			// -> Worker.run() will not write to log
			throw new CoreException(status);
		} else {
			// Status message could not be shown
			// -> Abort with original exception
			// -> Will write WARNINGs and ERRORs to log
			abort(message, exception, code);
		}
	}

	/**
	 * Collects all VM and program arguments. Implementors can modify and add
	 * arguments.
	 * 
	 * @param configuration
	 *            the configuration to collect the arguments for
	 * @param vmArguments
	 *            a {@link List} of {@link String} representing the resulting VM
	 *            arguments
	 * @param programArguments
	 *            a {@link List} of {@link String} representing the resulting
	 *            program arguments
	 * @exception CoreException
	 *                if unable to collect the execution arguments
	 */
	protected void collectExecutionArguments(
			ILaunchConfiguration configuration, List<String> vmArguments,
			List<String> programArguments) throws CoreException {

		// add program & VM arguments provided by getProgramArguments and
		// getVMArguments
		// String pgmArgs = getProgramArguments(configuration);
		// String vmArgs = getVMArguments(configuration);
		// ExecutionArguments execArgs = new ExecutionArguments(vmArgs,
		// pgmArgs);
		// vmArguments.addAll(Arrays.asList(execArgs.getVMArgumentsArray()));
		// programArguments.addAll(Arrays.asList(execArgs
		// .getProgramArgumentsArray()));

		String testFailureNames = configuration.getAttribute(
				GUnitLaunchConfigurationConstants.ATTR_FAILURES_NAMES, ""); //$NON-NLS-1$

		programArguments.add("-version"); //$NON-NLS-1$
		programArguments.add("3"); //$NON-NLS-1$

		programArguments.add("-port"); //$NON-NLS-1$
		programArguments.add(String.valueOf(this.fPort));

		if (this.fKeepAlive) {
			programArguments.add(0, "-keepalive"); //$NON-NLS-1$
		}

		ITestKind testRunnerKind = getTestRunnerKind(configuration);

		programArguments.add("-testLoaderClass"); //$NON-NLS-1$
		programArguments.add(testRunnerKind.getLoaderClassName());
		programArguments.add("-loaderpluginname"); //$NON-NLS-1$
		programArguments.add(testRunnerKind.getLoaderPluginId());

		IErlMember[] testElements = this.fTestElements;

		// a test name was specified just run the single test
		// if (testElements.length == 1) {
		// if (testElements[0] instanceof IMethod) {
		// IMethod method = (IMethod) testElements[0];
		//				programArguments.add("-test"); //$NON-NLS-1$
		// programArguments.add(method.getDeclaringType()
		// .getFullyQualifiedName()
		// + ':' + method.getElementName());
		// } else if (testElements[0] instanceof IErlModule) {
		// IErlModule type = (IErlModule) testElements[0];
		//				programArguments.add("-classNames"); //$NON-NLS-1$
		// programArguments.add(type.getFullyQualifiedName());
		// } else {
		// informAndAbort(
		// JUnitMessages.JUnitLaunchConfigurationDelegate_error_wrong_input,
		// null,
		// IJavaLaunchConfigurationConstants.ERR_UNSPECIFIED_MAIN_TYPE);
		// }
		// } else if (testElements.length > 1) {
		// String fileName = createTestNamesFile(testElements);
		//			programArguments.add("-testNameFile"); //$NON-NLS-1$
		// programArguments.add(fileName);
		// }
		// if (testFailureNames.length() > 0) {
		//			programArguments.add("-testfailures"); //$NON-NLS-1$
		// programArguments.add(testFailureNames);
		// }
	}

	private String createTestNamesFile(IErlMember[] testElements)
			throws CoreException {
		try {
			File file = File.createTempFile("testNames", ".txt"); //$NON-NLS-1$ //$NON-NLS-2$
			file.deleteOnExit();
			BufferedWriter bw = null;
			try {
				bw = new BufferedWriter(new FileWriter(file));
				for (int i = 0; i < testElements.length; i++) {
					if (testElements[i] instanceof IErlModule) {
						IErlModule type = (IErlModule) testElements[i];
						String testName = type.getName();
						bw.write(testName);
						bw.newLine();
					} else {
						// informAndAbort(
						// JUnitMessages.JUnitLaunchConfigurationDelegate_error_wrong_input,
						// null,
						// IJavaLaunchConfigurationConstants.ERR_UNSPECIFIED_MAIN_TYPE);
					}
				}
			} finally {
				if (bw != null) {
					bw.close();
				}
			}
			return file.getAbsolutePath();
		} catch (IOException e) {
			throw new CoreException(new Status(IStatus.ERROR,
					GUnitPlugin.PLUGIN_ID, IStatus.ERROR, "", e)); //$NON-NLS-1$
		}
	}

	// /*
	// * (non-Javadoc)
	// *
	// * @seeorg.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate#
	// * getClasspath(org.eclipse.debug.core.ILaunchConfiguration)
	// */
	// public String[] getClasspath(ILaunchConfiguration configuration)
	// throws CoreException {
	// String[] cp = super.getClasspath(configuration);
	//
	// ITestKind kind = getTestRunnerKind(configuration);
	// List junitEntries = new ClasspathLocalizer(Platform.inDevelopmentMode())
	// .localizeClasspath(kind);
	//
	// String[] classPath = new String[cp.length + junitEntries.size()];
	// Object[] jea = junitEntries.toArray();
	// System.arraycopy(cp, 0, classPath, 0, cp.length);
	// System.arraycopy(jea, 0, classPath, cp.length, jea.length);
	// return classPath;
	// }

	// private final IErlElement getTestTarget(
	// ILaunchConfiguration configuration, IErlProject javaProject)
	// throws CoreException {
	// String containerHandle = configuration.getAttribute(
	//				GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER, ""); //$NON-NLS-1$
	// if (containerHandle.length() != 0) {
	// IErlElement element = JavaCore.create(containerHandle);
	// if (element == null || !element.exists()) {
	// informAndAbort(
	// JUnitMessages.JUnitLaunchConfigurationDelegate_error_input_element_deosn_not_exist,
	// null,
	// IJavaLaunchConfigurationConstants.ERR_UNSPECIFIED_MAIN_TYPE);
	// }
	// return element;
	// }
	// String testTypeName = configuration.getAttribute(
	//				IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, ""); //$NON-NLS-1$
	// if (testTypeName.length() != 0) {
	// testTypeName = performStringSubstitution(testTypeName);
	// IErlModule type = javaProject.findType(testTypeName);
	// if (type != null && type.exists()) {
	// return type;
	// }
	// }
	// informAndAbort(
	// JUnitMessages.JUnitLaunchConfigurationDelegate_input_type_does_not_exist,
	// null,
	// IJavaLaunchConfigurationConstants.ERR_UNSPECIFIED_MAIN_TYPE);
	// return null; // not reachable
	// }

	private final String performStringSubstitution(String testTypeName)
			throws CoreException {
		return VariablesPlugin.getDefault().getStringVariableManager()
				.performStringSubstitution(testTypeName);
	}

	private boolean showStatusMessage(final IStatus status) {
		final boolean[] success = new boolean[] { false };
		getDisplay().syncExec(new Runnable() {
			public void run() {
				Shell shell = GUnitPlugin.getActiveWorkbenchShell();
				if (shell == null) {
					shell = getDisplay().getActiveShell();
				}
				if (shell != null) {
					MessageDialog
							.openInformation(
									shell,
									GUnitMessages.JUnitLaunchConfigurationDelegate_dialog_title,
									status.getMessage());
					success[0] = true;
				}
			}
		});
		return success[0];
	}

	private Display getDisplay() {
		Display display;
		display = Display.getCurrent();
		if (display == null) {
			display = Display.getDefault();
		}
		return display;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.gunit.internal.launcher.ITestFindingAbortHandler#abort(java
	 * .lang.String, java.lang.Throwable, int)
	 */
	protected void abort(String message, Throwable exception, int code)
			throws CoreException {
		throw new CoreException(new Status(IStatus.ERROR,
				GUnitPlugin.PLUGIN_ID, code, message, exception));
	}

}

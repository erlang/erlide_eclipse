package org.erlide.gunit.runners;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.debug.core.ILaunch;
import org.erlide.gunit.internal.model.TestElement;

public class AbstractTestRunner {

	// private Backend fBackend;

	public AbstractTestRunner(final ILaunch launch) {
		// try {
		// String projectName = launch.getLaunchConfiguration().getAttribute(
		// LaunchConfigurationConstants.ATTR_PROJECT_NAME, "");
		// String workingDirString = ResourcesPlugin.getWorkspace().getRoot()
		// .getProject(projectName).getLocation()
		// + "/ebin";
		// BackendManager backendManager = BackendManager.getDefault();
		//
		// String runtime = RuntimeInfoManager.getDefault()
		// .getDefaultRuntimeName();
		// final RuntimeInfo rt = RuntimeInfo.copy(ErlangCore
		// .getRuntimeInfoManager().getRuntime(runtime), false);
		// String nodeName = "dummy";
		// rt.setNodeName(nodeName);
		// rt.setCookie(null);
		//
		// final EnumSet<BackendOptions> options = EnumSet
		// .noneOf(BackendOptions.class);
		// options.add(BackendOptions.AUTOSTART);
		//
		// fBackend = backendManager.create(rt, options, launch);
		// System.out.println("Setting working directory: "
		// + fBackend.rpcx("c", "cd", "s", workingDirString));
		// String[] args = new String[] { "." };
		// System.out.println("Setting path: "
		// + fBackend.rpcx("code", "add_pathsz", "ls", (Object) args));
		// System.out.println("Setting environment: "
		// + fBackend.rpcx("os", "putenv", "ss",
		// "TEST_SERVER_FRAMEWORK", "shade"));
		// System.out.println("Environment = "
		// + fBackend.rpcx("os", "getenv", ""));
		// } catch (CoreException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// } catch (RpcException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// } catch (BackendException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// }
	}

	public List<TestElement> getTestCases(final TestElement testElement) {
		final List<TestElement> testCases = new ArrayList<TestElement>();

		// try {
		// OtpErlangList result = (OtpErlangList) fBackend.rpcx("gunit",
		// "get_test_cases", "a", testElement.getName());
		// OtpErlangObject[] testCaseAtomList = result.elements();
		// if (testCaseAtomList != null) { // UGLY, should by able to trust
		// // that an empty list is returned
		// // instead of null
		// for (int i = 0; i < testCaseAtomList.length; ++i) {
		// testCases.add(new TestCaseElement(
		// ((OtpErlangAtom) testCaseAtomList[i]).toString(),
		// testElement));
		// }
		// }
		// } catch (RpcException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// } catch (BackendException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// }
		//
		return testCases;
	}

	public TestElement.Status runTest(final TestElement testCase) {
		// try {
		// OtpErlangObject result = fBackend.rpcx("gunit", "run_test_case",
		// "aa", testCase.getParent().getName(), testCase.getName());
		//
		// System.out.println(result);
		//
		// } catch (RpcException e1) {
		// // TODO Auto-generated catch block
		// e1.printStackTrace();
		// } catch (BackendException e1) {
		// // TODO Auto-generated catch block
		// e1.printStackTrace();
		// }
		//
		// try {
		// Thread.sleep(2000);
		// } catch (InterruptedException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// }
		// if (Math.random() < 0.5) {
		// return TestElement.Status.OK;
		// } else {
		// return TestElement.Status.FAILURE;
		// }
		return TestElement.Status.OK;
	}

	public void shutDownErlangNode() {
		// try {
		// System.out.println("Terminating BT Erl Node: "
		// + fBackend.rpcx("c", "q", ""));
		// } catch (RpcException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// } catch (BackendException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// }
	}
}

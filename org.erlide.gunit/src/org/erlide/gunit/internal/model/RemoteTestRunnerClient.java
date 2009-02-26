/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Julien Ruaux: jruaux@octo.com 
 * 	   Vincent Massol: vmassol@octo.com 
 *******************************************************************************/
package org.erlide.gunit.internal.model;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;

import org.erlide.gunit.internal.ui.GUnitPlugin;

/**
 * The client side of the RemoteTestRunner. Handles the marshaling of the
 * different messages.
 */
public class RemoteTestRunnerClient {
	public abstract class ListenerSafeRunnable implements ISafeRunnable {
		public void handleException(Throwable exception) {
			GUnitPlugin.log(exception);
		}
	}

	/**
	 * A simple state machine to process requests from the RemoteTestRunner
	 */
	abstract class ProcessingState {
		abstract ProcessingState readMessage(String message);
	}

	class DefaultProcessingState extends ProcessingState {
		@Override
		ProcessingState readMessage(String message) {
			if (message.startsWith(MessageIds.TRACE_START)) {
				fFailedTrace.setLength(0);
				return fTraceState;
			}
			if (message.startsWith(MessageIds.EXPECTED_START)) {
				fExpectedResult.setLength(0);
				return fExpectedState;
			}
			if (message.startsWith(MessageIds.ACTUAL_START)) {
				fActualResult.setLength(0);
				return fActualState;
			}
			if (message.startsWith(MessageIds.RTRACE_START)) {
				fFailedRerunTrace.setLength(0);
				return fRerunState;
			}
			String arg = message.substring(MessageIds.MSG_HEADER_LENGTH);
			if (message.startsWith(MessageIds.TEST_RUN_START)) {
				// version < 2 format: count
				// version >= 2 format: count+" "+version
				int count = 0;
				int v = arg.indexOf(' ');
				if (v == -1) {
					fVersion = "v1"; //$NON-NLS-1$
					count = Integer.parseInt(arg);
				} else {
					fVersion = arg.substring(v + 1);
					String sc = arg.substring(0, v);
					count = Integer.parseInt(sc);
				}
				notifyTestRunStarted(count);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_START)) {
				notifyTestStarted(arg);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_END)) {
				notifyTestEnded(arg);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_ERROR)) {
				extractFailure(arg, ITestRunListener2.STATUS_ERROR);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_FAILED)) {
				extractFailure(arg, ITestRunListener2.STATUS_FAILURE);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_RUN_END)) {
				long elapsedTime = Long.parseLong(arg);
				testRunEnded(elapsedTime);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_STOPPED)) {
				long elapsedTime = Long.parseLong(arg);
				notifyTestRunStopped(elapsedTime);
				shutDown();
				return this;
			}
			if (message.startsWith(MessageIds.TEST_TREE)) {
				notifyTestTreeEntry(arg);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_RERAN)) {
				if (hasTestId())
					scanReranMessage(arg);
				else
					scanOldReranMessage(arg);
				return this;
			}
			return this;
		}
	}

	/**
	 * Base class for states in which messages are appended to an internal
	 * string buffer until an end message is read.
	 */
	class AppendingProcessingState extends ProcessingState {
		private final StringBuffer fBuffer;
		private String fEndString;

		AppendingProcessingState(StringBuffer buffer, String endString) {
			this.fBuffer = buffer;
			this.fEndString = endString;
		}

		@Override
		ProcessingState readMessage(String message) {
			if (message.startsWith(fEndString)) {
				entireStringRead();
				return fDefaultState;
			}
			fBuffer.append(message);
			fBuffer.append('\n');
			return this;
		}

		/**
		 * subclasses can override to do special things when end message is read
		 */
		void entireStringRead() {
		}
	}

	class TraceProcessingState extends AppendingProcessingState {
		TraceProcessingState() {
			super(fFailedTrace, MessageIds.TRACE_END);
		}

		@Override
		void entireStringRead() {
			notifyTestFailed();
			fExpectedResult.setLength(0);
			fActualResult.setLength(0);
		}

		@Override
		ProcessingState readMessage(String message) {
			if (message.startsWith(MessageIds.TRACE_END)) {
				notifyTestFailed();
				fFailedTrace.setLength(0);
				fActualResult.setLength(0);
				fExpectedResult.setLength(0);
				return fDefaultState;
			}
			fFailedTrace.append(message).append('\n');
			return this;
		}
	}

	/**
	 * The failed trace that is currently reported from the RemoteTestRunner
	 */
	private final StringBuffer fFailedTrace = new StringBuffer();
	/**
	 * The expected test result
	 */
	private final StringBuffer fExpectedResult = new StringBuffer();
	/**
	 * The actual test result
	 */
	private final StringBuffer fActualResult = new StringBuffer();
	/**
	 * The failed trace of a reran test
	 */
	private final StringBuffer fFailedRerunTrace = new StringBuffer();

	ProcessingState fDefaultState = new DefaultProcessingState();
	ProcessingState fTraceState = new TraceProcessingState();
	ProcessingState fExpectedState = new AppendingProcessingState(
			fExpectedResult, MessageIds.EXPECTED_END);
	ProcessingState fActualState = new AppendingProcessingState(fActualResult,
			MessageIds.ACTUAL_END);
	ProcessingState fRerunState = new AppendingProcessingState(
			fFailedRerunTrace, MessageIds.RTRACE_END);
	ProcessingState fCurrentState = fDefaultState;

	/**
	 * An array of listeners that are informed about test events.
	 */
	private ITestRunListener2[] fListeners;

	/**
	 * The server socket
	 */
	private ServerSocket fServerSocket;
	private Socket fSocket;
	private int fPort = -1;
	private PrintWriter fWriter;
	private BufferedReader fBufferedReader;
	/**
	 * The protocol version
	 */
	private String fVersion;
	/**
	 * The failed test that is currently reported from the RemoteTestRunner
	 */
	private String fFailedTest;
	/**
	 * The Id of the failed test
	 */
	private String fFailedTestId;
	/**
	 * The kind of failure of the test that is currently reported as failed
	 */
	private int fFailureKind;

	private boolean fDebug = false;

	/**
	 * Reads the message stream from the RemoteTestRunner
	 */
	private class ServerConnection extends Thread {
		int fServerPort;

		public ServerConnection(int port) {
			super("ServerConnection"); //$NON-NLS-1$
			fServerPort = port;
		}

		@Override
		public void run() {
			try {
				if (fDebug)
					System.out.println("Creating server socket " + fServerPort); //$NON-NLS-1$
				fServerSocket = new ServerSocket(fServerPort);
				fSocket = fServerSocket.accept();
				try {
					fBufferedReader = new BufferedReader(new InputStreamReader(
							fSocket.getInputStream(), "UTF-8")); //$NON-NLS-1$
				} catch (UnsupportedEncodingException e) {
					fBufferedReader = new BufferedReader(new InputStreamReader(
							fSocket.getInputStream()));
				}
				try {
					fWriter = new PrintWriter(new OutputStreamWriter(fSocket
							.getOutputStream(), "UTF-8"), true); //$NON-NLS-1$
				} catch (UnsupportedEncodingException e1) {
					fWriter = new PrintWriter(new OutputStreamWriter(fSocket
							.getOutputStream()), true);
				}
				String message;
				while (fBufferedReader != null
						&& (message = readMessage(fBufferedReader)) != null)
					receiveMessage(message);
			} catch (SocketException e) {
				notifyTestRunTerminated();
			} catch (IOException e) {
				GUnitPlugin.log(e);
				// fall through
			}
			shutDown();
		}
	}

	/**
	 * Start listening to a test run. Start a server connection that the
	 * RemoteTestRunner can connect to.
	 * 
	 * @param listeners
	 * @param port
	 */
	public synchronized void startListening(ITestRunListener2[] listeners,
			int port) {
		fListeners = listeners;
		fPort = port;
		ServerConnection connection = new ServerConnection(port);
		connection.start();
	}

	/**
	 * Requests to stop the remote test run.
	 */
	public synchronized void stopTest() {
		if (isRunning()) {
			fWriter.println(MessageIds.TEST_STOP);
			fWriter.flush();
		}
	}

	public synchronized void stopWaiting() {
		if (fServerSocket != null && !fServerSocket.isClosed()
				&& fSocket == null) {
			shutDown(); // will throw a SocketException in Threads that wait in
						// ServerSocket#accept()
		}
	}

	private synchronized void shutDown() {
		if (fDebug)
			System.out.println("shutdown " + fPort); //$NON-NLS-1$

		if (fWriter != null) {
			fWriter.close();
			fWriter = null;
		}
		try {
			if (fBufferedReader != null) {
				fBufferedReader.close();
				fBufferedReader = null;
			}
		} catch (IOException e) {
		}
		try {
			if (fSocket != null) {
				fSocket.close();
				fSocket = null;
			}
		} catch (IOException e) {
		}
		try {
			if (fServerSocket != null) {
				fServerSocket.close();
				fServerSocket = null;
			}
		} catch (IOException e) {
		}
	}

	public boolean isRunning() {
		return fSocket != null;
	}

	private String readMessage(BufferedReader in) throws IOException {
		return in.readLine();
	}

	private void receiveMessage(String message) {
		fCurrentState = fCurrentState.readMessage(message);
	}

	private void scanOldReranMessage(String arg) {
		// OLD V1 format
		// format: className" "testName" "status
		// status: FAILURE, ERROR, OK
		int c = arg.indexOf(" "); //$NON-NLS-1$
		int t = arg.indexOf(" ", c + 1); //$NON-NLS-1$
		String className = arg.substring(0, c);
		String testName = arg.substring(c + 1, t);
		String status = arg.substring(t + 1);
		String testId = className + testName;
		notifyTestReran(testId, className, testName, status);
	}

	private void scanReranMessage(String arg) {
		// format: testId" "className" "testName" "status
		// status: FAILURE, ERROR, OK
		int i = arg.indexOf(' ');
		int c = arg.indexOf(' ', i + 1);
		int t = arg.indexOf(' ', c + 1);
		String testId = arg.substring(0, i);
		String className = arg.substring(i + 1, c);
		String testName = arg.substring(c + 1, t);
		String status = arg.substring(t + 1);
		notifyTestReran(testId, className, testName, status);
	}

	private void notifyTestReran(String testId, String className,
			String testName, String status) {
		int statusCode = ITestRunListener2.STATUS_OK;
		if (status.equals("FAILURE")) //$NON-NLS-1$
			statusCode = ITestRunListener2.STATUS_FAILURE;
		else if (status.equals("ERROR")) //$NON-NLS-1$
			statusCode = ITestRunListener2.STATUS_ERROR;

		String trace = ""; //$NON-NLS-1$
		if (statusCode != ITestRunListener2.STATUS_OK)
			trace = fFailedRerunTrace.toString();
		// assumption a rerun trace was sent before
		notifyTestReran(testId, className, testName, statusCode, trace);
	}

	private void extractFailure(String arg, int status) {
		String s[] = extractTestId(arg);
		fFailedTestId = s[0];
		fFailedTest = s[1];
		fFailureKind = status;
	}

	/**
	 * @param arg
	 *            test name
	 * @return an array with two elements. The first one is the testId, the
	 *         second one the testName.
	 */
	String[] extractTestId(String arg) {
		String[] result = new String[2];
		if (!hasTestId()) {
			result[0] = arg; // use the test name as the test Id
			result[1] = arg;
			return result;
		}
		int i = arg.indexOf(',');
		result[0] = arg.substring(0, i);
		result[1] = arg.substring(i + 1, arg.length());
		return result;
	}

	private boolean hasTestId() {
		if (fVersion == null) // TODO fix me
			return true;
		return fVersion.equals("v2"); //$NON-NLS-1$
	}

	private void notifyTestReran(final String testId, final String className,
			final String testName, final int statusCode, final String trace) {
		for (int i = 0; i < fListeners.length; i++) {
			final ITestRunListener2 listener = fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testReran(testId, className, testName, statusCode,
							trace, fExpectedResult.toString(), fActualResult
									.toString());
				}
			});
		}
	}

	private void notifyTestTreeEntry(final String treeEntry) {
		for (int i = 0; i < fListeners.length; i++) {
			ITestRunListener2 listener = fListeners[i];
			if (!hasTestId())
				listener.testTreeEntry(fakeTestId(treeEntry));
			else
				listener.testTreeEntry(treeEntry);
		}
	}

	private String fakeTestId(String treeEntry) {
		// extract the test name and add it as the testId
		int index0 = treeEntry.indexOf(',');
		String testName = treeEntry.substring(0, index0).trim();
		return testName + "," + treeEntry; //$NON-NLS-1$
	}

	private void notifyTestRunStopped(final long elapsedTime) {
		if (GUnitPlugin.isStopped())
			return;
		for (int i = 0; i < fListeners.length; i++) {
			final ITestRunListener2 listener = fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testRunStopped(elapsedTime);
				}
			});
		}
	}

	private void testRunEnded(final long elapsedTime) {
		if (GUnitPlugin.isStopped())
			return;
		for (int i = 0; i < fListeners.length; i++) {
			final ITestRunListener2 listener = fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testRunEnded(elapsedTime);
				}
			});
		}
	}

	private void notifyTestEnded(final String test) {
		if (GUnitPlugin.isStopped())
			return;
		for (int i = 0; i < fListeners.length; i++) {
			final ITestRunListener2 listener = fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					String s[] = extractTestId(test);
					listener.testEnded(s[0], s[1]);
				}
			});
		}
	}

	private void notifyTestStarted(final String test) {
		if (GUnitPlugin.isStopped())
			return;
		for (int i = 0; i < fListeners.length; i++) {
			final ITestRunListener2 listener = fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					String s[] = extractTestId(test);
					listener.testStarted(s[0], s[1]);
				}
			});
		}
	}

	private void notifyTestRunStarted(final int count) {
		if (GUnitPlugin.isStopped())
			return;
		for (int i = 0; i < fListeners.length; i++) {
			final ITestRunListener2 listener = fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testRunStarted(count);
				}
			});
		}
	}

	private void notifyTestFailed() {
		if (GUnitPlugin.isStopped())
			return;
		for (int i = 0; i < fListeners.length; i++) {
			final ITestRunListener2 listener = fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testFailed(fFailureKind, fFailedTestId,
							fFailedTest, fFailedTrace.toString(),
							fExpectedResult.toString(), fActualResult
									.toString());
				}
			});
		}
	}

	private void notifyTestRunTerminated() {
		// fix for 77771 RemoteTestRunnerClient doing work after junit shutdown
		// [JUnit]
		if (GUnitPlugin.isStopped())
			return;
		for (int i = 0; i < fListeners.length; i++) {
			final ITestRunListener2 listener = fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testRunTerminated();
				}
			});
		}
	}

	public void rerunTest(String testId, String className, String testName) {
		if (isRunning()) {
			fActualResult.setLength(0);
			fExpectedResult.setLength(0);
			fWriter.println(MessageIds.TEST_RERUN + testId
					+ " " + className + " " + testName); //$NON-NLS-1$ //$NON-NLS-2$
			fWriter.flush();
		}
	}
}

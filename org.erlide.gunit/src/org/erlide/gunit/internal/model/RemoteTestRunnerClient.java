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
		public void handleException(final Throwable exception) {
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
		ProcessingState readMessage(final String message) {
			if (message.startsWith(MessageIds.TRACE_START)) {
				RemoteTestRunnerClient.this.fFailedTrace.setLength(0);
				return RemoteTestRunnerClient.this.fTraceState;
			}
			if (message.startsWith(MessageIds.EXPECTED_START)) {
				RemoteTestRunnerClient.this.fExpectedResult.setLength(0);
				return RemoteTestRunnerClient.this.fExpectedState;
			}
			if (message.startsWith(MessageIds.ACTUAL_START)) {
				RemoteTestRunnerClient.this.fActualResult.setLength(0);
				return RemoteTestRunnerClient.this.fActualState;
			}
			if (message.startsWith(MessageIds.RTRACE_START)) {
				RemoteTestRunnerClient.this.fFailedRerunTrace.setLength(0);
				return RemoteTestRunnerClient.this.fRerunState;
			}
			final String arg = message.substring(MessageIds.MSG_HEADER_LENGTH);
			if (message.startsWith(MessageIds.TEST_RUN_START)) {
				// version < 2 format: count
				// version >= 2 format: count+" "+version
				int count = 0;
				final int v = arg.indexOf(' ');
				if (v == -1) {
					RemoteTestRunnerClient.this.fVersion = "v1"; //$NON-NLS-1$
					count = Integer.parseInt(arg);
				} else {
					RemoteTestRunnerClient.this.fVersion = arg.substring(v + 1);
					final String sc = arg.substring(0, v);
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
				final long elapsedTime = Long.parseLong(arg);
				testRunEnded(elapsedTime);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_STOPPED)) {
				final long elapsedTime = Long.parseLong(arg);
				notifyTestRunStopped(elapsedTime);
				shutDown();
				return this;
			}
			if (message.startsWith(MessageIds.TEST_TREE)) {
				notifyTestTreeEntry(arg);
				return this;
			}
			if (message.startsWith(MessageIds.TEST_RERAN)) {
				if (hasTestId()) {
					scanReranMessage(arg);
				} else {
					scanOldReranMessage(arg);
				}
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

		private final String fEndString;

		AppendingProcessingState(final StringBuffer buffer, final String endString) {
			this.fBuffer = buffer;
			this.fEndString = endString;
		}

		@Override
		ProcessingState readMessage(final String message) {
			if (message.startsWith(this.fEndString)) {
				entireStringRead();
				return RemoteTestRunnerClient.this.fDefaultState;
			}
			this.fBuffer.append(message);
			this.fBuffer.append('\n');
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
			super(RemoteTestRunnerClient.this.fFailedTrace,
					MessageIds.TRACE_END);
		}

		@Override
		void entireStringRead() {
			notifyTestFailed();
			RemoteTestRunnerClient.this.fExpectedResult.setLength(0);
			RemoteTestRunnerClient.this.fActualResult.setLength(0);
		}

		@Override
		ProcessingState readMessage(final String message) {
			if (message.startsWith(MessageIds.TRACE_END)) {
				notifyTestFailed();
				RemoteTestRunnerClient.this.fFailedTrace.setLength(0);
				RemoteTestRunnerClient.this.fActualResult.setLength(0);
				RemoteTestRunnerClient.this.fExpectedResult.setLength(0);
				return RemoteTestRunnerClient.this.fDefaultState;
			}
			RemoteTestRunnerClient.this.fFailedTrace.append(message).append(
			'\n');
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
			this.fExpectedResult, MessageIds.EXPECTED_END);

	ProcessingState fActualState = new AppendingProcessingState(
			this.fActualResult, MessageIds.ACTUAL_END);

	ProcessingState fRerunState = new AppendingProcessingState(
			this.fFailedRerunTrace, MessageIds.RTRACE_END);

	ProcessingState fCurrentState = this.fDefaultState;

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

	private final boolean fDebug = false;

	/**
	 * Reads the message stream from the RemoteTestRunner
	 */
	private class ServerConnection extends Thread {
		int fServerPort;

		public ServerConnection(final int port) {
			super("ServerConnection"); //$NON-NLS-1$
			this.fServerPort = port;
		}

		@Override
		public void run() {
			try {
				if (RemoteTestRunnerClient.this.fDebug) {
					System.out
					.println("Creating server socket " + this.fServerPort); //$NON-NLS-1$
				}
				RemoteTestRunnerClient.this.fServerSocket = new ServerSocket(
						this.fServerPort);
				RemoteTestRunnerClient.this.fSocket = RemoteTestRunnerClient.this.fServerSocket
				.accept();
				try {
					RemoteTestRunnerClient.this.fBufferedReader = new BufferedReader(
							new InputStreamReader(
									RemoteTestRunnerClient.this.fSocket
									.getInputStream(), "UTF-8")); //$NON-NLS-1$
				} catch (final UnsupportedEncodingException e) {
					RemoteTestRunnerClient.this.fBufferedReader = new BufferedReader(
							new InputStreamReader(
									RemoteTestRunnerClient.this.fSocket
									.getInputStream()));
				}
				try {
					RemoteTestRunnerClient.this.fWriter = new PrintWriter(
							new OutputStreamWriter(
									RemoteTestRunnerClient.this.fSocket
									.getOutputStream(), "UTF-8"), true); //$NON-NLS-1$
				} catch (final UnsupportedEncodingException e1) {
					RemoteTestRunnerClient.this.fWriter = new PrintWriter(
							new OutputStreamWriter(
									RemoteTestRunnerClient.this.fSocket
									.getOutputStream()), true);
				}
				String message;
				while (RemoteTestRunnerClient.this.fBufferedReader != null
						&& (message = readMessage(RemoteTestRunnerClient.this.fBufferedReader)) != null) {
					receiveMessage(message);
				}
			} catch (final SocketException e) {
				notifyTestRunTerminated();
			} catch (final IOException e) {
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
	public synchronized void startListening(final ITestRunListener2[] listeners,
			final int port) {
		this.fListeners = listeners;
		this.fPort = port;
		final ServerConnection connection = new ServerConnection(port);
		connection.start();
	}

	/**
	 * Requests to stop the remote test run.
	 */
	public synchronized void stopTest() {
		if (isRunning()) {
			this.fWriter.println(MessageIds.TEST_STOP);
			this.fWriter.flush();
		}
	}

	public synchronized void stopWaiting() {
		if (this.fServerSocket != null && !this.fServerSocket.isClosed()
				&& this.fSocket == null) {
			shutDown(); // will throw a SocketException in Threads that wait in
			// ServerSocket#accept()
		}
	}

	private synchronized void shutDown() {
		if (this.fDebug) {
			System.out.println("shutdown " + this.fPort); //$NON-NLS-1$
		}

		if (this.fWriter != null) {
			this.fWriter.close();
			this.fWriter = null;
		}
		try {
			if (this.fBufferedReader != null) {
				this.fBufferedReader.close();
				this.fBufferedReader = null;
			}
		} catch (final IOException e) {
		}
		try {
			if (this.fSocket != null) {
				this.fSocket.close();
				this.fSocket = null;
			}
		} catch (final IOException e) {
		}
		try {
			if (this.fServerSocket != null) {
				this.fServerSocket.close();
				this.fServerSocket = null;
			}
		} catch (final IOException e) {
		}
	}

	public boolean isRunning() {
		return this.fSocket != null;
	}

	private String readMessage(final BufferedReader in) throws IOException {
		return in.readLine();
	}

	private void receiveMessage(final String message) {
		this.fCurrentState = this.fCurrentState.readMessage(message);
	}

	private void scanOldReranMessage(final String arg) {
		// OLD V1 format
		// format: className" "testName" "status
		// status: FAILURE, ERROR, OK
		final int c = arg.indexOf(" "); //$NON-NLS-1$
		final int t = arg.indexOf(" ", c + 1); //$NON-NLS-1$
		final String className = arg.substring(0, c);
		final String testName = arg.substring(c + 1, t);
		final String status = arg.substring(t + 1);
		final String testId = className + testName;
		notifyTestReran(testId, className, testName, status);
	}

	private void scanReranMessage(final String arg) {
		// format: testId" "className" "testName" "status
		// status: FAILURE, ERROR, OK
		final int i = arg.indexOf(' ');
		final int c = arg.indexOf(' ', i + 1);
		final int t = arg.indexOf(' ', c + 1);
		final String testId = arg.substring(0, i);
		final String className = arg.substring(i + 1, c);
		final String testName = arg.substring(c + 1, t);
		final String status = arg.substring(t + 1);
		notifyTestReran(testId, className, testName, status);
	}

	private void notifyTestReran(final String testId, final String className,
			final String testName, final String status) {
		int statusCode = ITestRunListener2.STATUS_OK;
		if (status.equals("FAILURE")) {
			statusCode = ITestRunListener2.STATUS_FAILURE;
		} else if (status.equals("ERROR")) {
			statusCode = ITestRunListener2.STATUS_ERROR;
		}

		String trace = ""; //$NON-NLS-1$
		if (statusCode != ITestRunListener2.STATUS_OK) {
			trace = this.fFailedRerunTrace.toString();
		}
		// assumption a rerun trace was sent before
		notifyTestReran(testId, className, testName, statusCode, trace);
	}

	private void extractFailure(final String arg, final int status) {
		final String s[] = extractTestId(arg);
		this.fFailedTestId = s[0];
		this.fFailedTest = s[1];
		this.fFailureKind = status;
	}

	/**
	 * @param arg
	 *            test name
	 * @return an array with two elements. The first one is the testId, the
	 *         second one the testName.
	 */
	String[] extractTestId(final String arg) {
		final String[] result = new String[2];
		if (!hasTestId()) {
			result[0] = arg; // use the test name as the test Id
			result[1] = arg;
			return result;
		}
		final int i = arg.indexOf(',');
		result[0] = arg.substring(0, i);
		result[1] = arg.substring(i + 1, arg.length());
		return result;
	}

	private boolean hasTestId() {
		if (this.fVersion == null) {
			return true;
		}
		return this.fVersion.equals("v2"); //$NON-NLS-1$
	}

	private void notifyTestReran(final String testId, final String className,
			final String testName, final int statusCode, final String trace) {
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testReran(testId, className, testName, statusCode,
							trace, RemoteTestRunnerClient.this.fExpectedResult
							.toString(),
							RemoteTestRunnerClient.this.fActualResult
							.toString());
				}
			});
		}
	}

	private void notifyTestTreeEntry(final String treeEntry) {
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			if (!hasTestId()) {
				listener.testTreeEntry(fakeTestId(treeEntry));
			} else {
				listener.testTreeEntry(treeEntry);
			}
		}
	}

	private String fakeTestId(final String treeEntry) {
		// extract the test name and add it as the testId
		final int index0 = treeEntry.indexOf(',');
		final String testName = treeEntry.substring(0, index0).trim();
		return testName + "," + treeEntry; //$NON-NLS-1$
	}

	private void notifyTestRunStopped(final long elapsedTime) {
		if (GUnitPlugin.isStopped()) {
			return;
		}
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testRunStopped(elapsedTime);
				}
			});
		}
	}

	private void testRunEnded(final long elapsedTime) {
		if (GUnitPlugin.isStopped()) {
			return;
		}
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testRunEnded(elapsedTime);
				}
			});
		}
	}

	private void notifyTestEnded(final String test) {
		if (GUnitPlugin.isStopped()) {
			return;
		}
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					final String s[] = extractTestId(test);
					listener.testEnded(s[0], s[1]);
				}
			});
		}
	}

	private void notifyTestStarted(final String test) {
		if (GUnitPlugin.isStopped()) {
			return;
		}
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					final String s[] = extractTestId(test);
					listener.testStarted(s[0], s[1]);
				}
			});
		}
	}

	private void notifyTestRunStarted(final int count) {
		if (GUnitPlugin.isStopped()) {
			return;
		}
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testRunStarted(count);
				}
			});
		}
	}

	private void notifyTestFailed() {
		if (GUnitPlugin.isStopped()) {
			return;
		}
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener
					.testFailed(
							RemoteTestRunnerClient.this.fFailureKind,
							RemoteTestRunnerClient.this.fFailedTestId,
							RemoteTestRunnerClient.this.fFailedTest,
							RemoteTestRunnerClient.this.fFailedTrace
							.toString(),
							RemoteTestRunnerClient.this.fExpectedResult
							.toString(),
							RemoteTestRunnerClient.this.fActualResult
							.toString());
				}
			});
		}
	}

	private void notifyTestRunTerminated() {
		// fix for 77771 RemoteTestRunnerClient doing work after junit shutdown
		// [JUnit]
		if (GUnitPlugin.isStopped()) {
			return;
		}
		for (int i = 0; i < this.fListeners.length; i++) {
			final ITestRunListener2 listener = this.fListeners[i];
			SafeRunner.run(new ListenerSafeRunnable() {
				public void run() {
					listener.testRunTerminated();
				}
			});
		}
	}

	public void rerunTest(final String testId, final String className, final String testName) {
		if (isRunning()) {
			this.fActualResult.setLength(0);
			this.fExpectedResult.setLength(0);
			this.fWriter.println(MessageIds.TEST_RERUN + testId
					+ " " + className + " " + testName); //$NON-NLS-1$ //$NON-NLS-2$
			this.fWriter.flush();
		}
	}
}

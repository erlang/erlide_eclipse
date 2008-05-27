/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.debug;

import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.rpc.ErlEventLoop;
import org.erlide.jinterface.rpc.IErlEventHandler;
import org.erlide.runtime.backend.IBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DebuggerListener {

	ErlangDebugTarget fTarget;

	IBackend fBackend;

	OtpErlangPid fDbgPid;

	private final ErlEventLoop loop;

	public DebuggerListener(final String name, final ErlangDebugTarget target,
			final IBackend b, final OtpErlangPid dbgPid) {
		fTarget = target;
		fBackend = b;
		fDbgPid = dbgPid;

		// TODO use the new event router job!

		final IErlEventHandler r = new DebuggerJob();
		loop = new ErlEventLoop(r);
		loop.start();
	}

	public class DebuggerJob implements IErlEventHandler {

		private OtpErlangPid self;

		public void init() {
			self = fBackend.getEventPid();

			fBackend.send(fDbgPid, new OtpErlangTuple(new OtpErlangAtom(
					"parent"), self));
		}

		public void handleEvent(final OtpErlangObject msg) {
			if (msg != null) {
				ErlLogger.debug("### got msg: " + msg);
			}
		}

		public boolean isTerminated() {
			return fTarget.isTerminated();
		}

		public OtpErlangObject receiveEvent(final int timeout)
				throws OtpErlangExit, OtpErlangDecodeException {
			if (timeout < 0) {
				// TODO how to handle stopping, if it's blocked in a receive?
				return fBackend.receiveRpc(60000);
			}
			return fBackend.receiveRpc(timeout);
		}

		public int getTimeout() {
			return 200;
		}

		public boolean exception(final Exception e) {
			e.printStackTrace();
			return false;
		}

	}

	public void stop() {
		loop.stop();
	}

}

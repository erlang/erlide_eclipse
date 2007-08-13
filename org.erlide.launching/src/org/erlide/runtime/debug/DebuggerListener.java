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
import org.erlide.jinterface.ErlEventLoop;
import org.erlide.jinterface.IErlEventHandler;
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

	private ErlEventLoop loop;

	public DebuggerListener(String name, ErlangDebugTarget target, IBackend b,
			OtpErlangPid dbgPid) {
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

			fBackend.send(fDbgPid, new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("parent"), self }));
		}

		public void handleEvent(OtpErlangObject msg) {
			if (msg != null) {
				ErlLogger.log("### got msg: " + msg);
			}
		}

		public boolean isTerminated() {
			return fTarget.isTerminated();
		}

		public OtpErlangObject receiveEvent(int timeout) throws OtpErlangExit,
				OtpErlangDecodeException {
			if (timeout < 0) {
				// TODO how to handle stopping, if it's blocked in a receive?
				return fBackend.receiveEvent();
			} else {
				return fBackend.receiveRpc(timeout);
			}
		}

		public int getTimeout() {
			return 200;
		}

		public boolean exception(Exception e) {
			e.printStackTrace();
			return false;
		}

	}

	public void stop() {
		loop.stop();
	}

}

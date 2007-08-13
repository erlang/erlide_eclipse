/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.IOException;
import java.util.List;

import org.eclipse.debug.core.IStreamListener;
import org.erlide.runtime.backend.console.BackendShellManager;
import org.erlide.runtime.backend.exceptions.ErlangParseException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface IBackend {

	public static final String ERL_BACKEND = "erlide_backend";

	public static final String MANAGED_BACKEND = "managed";

	public static final String STANDALONE_BACKEND = "standalone";

	void addProject(String project);

	/**
	 * Method dispose
	 */
	void dispose();

	/**
	 * @throws ErlangRpcException
	 * 
	 * @param m
	 * @param f
	 * @return OtpErlangObject
	 */
	RpcResult rpc(String m, String f) throws ErlangRpcException;

	/**
	 * @throws ErlangRpcException
	 * 
	 * @param m
	 * @param f
	 * @param a
	 * @return OtpErlangObject
	 */
	RpcResult rpc(String m, String f, OtpErlangObject... a)
			throws ErlangRpcException;

	/**
	 * @throws ErlangRpcException
	 * @param m
	 * @param f
	 * @param a
	 * @return OtpErlangObject
	 * @throws ErlangParseException
	 */
	RpcResult rpc(String m, String f, String[] a) throws ErlangParseException,
			ErlangRpcException;

	/**
	 */
	RpcResult rpc(String m, String f, OtpErlangObject[] a, int timeout)
			throws ErlangRpcException;

	/**
	 * 
	 * @param pid
	 * @param msg
	 */
	void send(OtpErlangPid pid, OtpErlangObject msg);

	void send(String name, OtpErlangObject msg);

	/**
	 * Method addEventListener
	 * 
	 * @param event
	 *            String
	 * @param l
	 *            IBackendEventListener
	 */
	void addEventListener(String event, IBackendEventListener l);

	/**
	 * Method removeEventListener
	 * 
	 * @param event
	 *            String
	 * @param l
	 *            IBackendEventListener
	 */
	void removeEventListener(String event, IBackendEventListener l);

	ICodeManager getCodeManager();

	OtpErlangPid getEventPid();

	OtpErlangPid getRpcPid();

	String getCurrentVersion();

	OtpErlangObject receive(int i) throws OtpErlangExit,
			OtpErlangDecodeException;

	OtpErlangObject receiveEvent() throws OtpErlangExit,
			OtpErlangDecodeException;

	OtpErlangObject receiveRpc(long timeout) throws OtpErlangExit,
			OtpErlangDecodeException;

	OtpErlangObject execute(String fun, OtpErlangObject[] args)
			throws ErlangRpcException;

	BackendShellManager getShellManager();

	void sendToDefaultShell(String msg) throws IOException;

	void sendToShell(String str);

	void addStdListener(IStreamListener dsp);

	boolean isManaged();

	String getLabel();

	boolean ping();

	ErlRpcDaemon getRpcDaemon();

	List<IBackendEventListener> getEventListeners(String event);
}

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

import org.eclipse.debug.core.model.IProcess;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.console.IShellManager;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface IBackend {

	/**
	 * @see #rpc(String, String, int, String, Object...)
	 */
	RpcResult rpc(String m, String f, String signature, Object... a)
			throws RpcException;

	// TODO signature specification is not complete!
	/**
	 * Makes a RPC to the Erlang side. || subject to changes! ||
	 * 
	 * <p>
	 * The signature specifies the types of the Erlang arguments, so that the
	 * Java ones can be converted automatically. It is the concatenation of
	 * signatures for each argument as defined below:
	 * <dl>
	 * <dt>x</dt>
	 * <dd>Generic; a simple conversion is done:
	 * <ul>
	 * <li>integer types -> integer</li>
	 * <li>String -> string</li>
	 * <li>List<> -> list</li>
	 * <li>array -> list</li>
	 * </ul>
	 * </dd>
	 * <dt>i</dt>
	 * <dd>any integral type, including char -> integer()</dd>
	 * <dt>d</dt>
	 * <dd>floats and doubles -> float()</dd>
	 * <dt>s</dt>
	 * <dd>String -> string().</dd>
	 * <dt>a</dt>
	 * <dd>String -> atom()</dd>
	 * <dt>b</dt>
	 * <dd>String, byte[] -> binary()</dd>
	 * <dt>o</dt>
	 * <dd>boolean -> atom()</dd>
	 * <dt>p</dt>
	 * <dd>OtpErlangPid.</dd>
	 * <dt>r</dt>
	 * <dd>OtpErlangReference.</dd>
	 * <dt>l*</dt>
	 * <dd>List<>, array -> list(). * is the elements' signature</dd>
	 * <dt>t***</dt>
	 * <dd>Object[] -> tuple(); *** are the tuple elements' signatures</dd>
	 * <dt>j</dt>
	 * <dd>java object reference; see below.</dd>
	 * </dl>
	 * Object instances not covered here are handled in a special way: they are
	 * stored in a weak map keyed by OtpErlangReference and the reference is
	 * sent instead. This reference can later be used to make RPC calls from
	 * Erlang to Java.
	 * </p>
	 * <p>
	 * Example: "salstaxls" means "string(), atom(), [string()], {atom(),
	 * term(), [string()]}" and the rpc's arguments can be of types String,
	 * String, List<String>, Object[]
	 * </p>
	 * 
	 * @param m
	 *            module name
	 * @param f
	 *            function name
	 * @param timeout
	 *            timeout for the call
	 * @param signature
	 *            the arguments' signature as above
	 * @param args
	 *            the arguments
	 * 
	 * @return the result of the RPC
	 * 
	 * @throws RpcException
	 *             the rpc exception
	 */
	RpcResult rpc(String m, String f, int timeout, String signature,
			Object... a) throws RpcException;

	/**
	 * @see #rpc(String, String, int, String, Object...)
	 * @throws BackendException
	 * @throws RpcException
	 */
	OtpErlangObject rpcx(String m, String f, String signature, Object... a)
			throws RpcException, BackendException;

	/**
	 * @see #rpc(String, String, int, String, Object...)
	 * @throws BackendException
	 * @throws RpcException
	 */
	OtpErlangObject rpcx(String m, String f, int timeout, String signature,
			Object... a) throws RpcException, BackendException;

	/**
	 * 
	 * @param pid
	 * @param msg
	 * @param signature
	 *            TODO
	 * @throws ConversionException
	 */
	void send(OtpErlangPid pid, Object msg);

	void send(String name, Object msg);

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

	OtpErlangObject receiveRpc(long timeout) throws OtpErlangExit,
			OtpErlangDecodeException;

	IShellManager getShellManager();

	void sendToDefaultShell(String msg) throws IOException;

	// void sendToShell(String str);

	// void addStdListener(IStreamListener dsp);

	// void setLabel(final String label);

	boolean ping();

	List<IBackendEventListener> getEventListeners(String event);

	RuntimeInfo getInfo();

	void setRuntime(final IProcess process);

	void setRemoteRex(OtpErlangPid rex);

	abstract void initializeRuntime();

	void connectAndRegister(final List<ICodeBundle> plugins);

	IdeBackend asIDE();

	BuildBackend asBuild();

	ExecutionBackend asExecution();

}

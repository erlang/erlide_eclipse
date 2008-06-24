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
import org.eclipse.debug.core.model.IProcess;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.console.IShellManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface IBackend {

	/**
	 * @throws ErlangRpcException
	 * 
	 * @param m
	 * @param f
	 * @param signature
	 *            TODO
	 * @param a
	 * @return OtpErlangObject
	 * @throws RpcException
	 * @throws ConversionException
	 */
	RpcResult rpc(String m, String f, String signature, Object... a)
			throws RpcException;

	/**
	 * @throws ErlangRpcException
	 * 
	 * @param m
	 * @param f
	 * @param timeout
	 * @param signature
	 *            TODO
	 * @param a
	 * @return OtpErlangObject
	 * @throws NoBackendException
	 * @throws ConversionException
	 */
	RpcResult rpc(String m, String f, int timeout, String signature,
			Object... a) throws RpcException;

	/**
	 * @throws ErlangRpcException,
	 *             BackendException
	 * 
	 * @param m
	 * @param f
	 * @param signature
	 *            TODO
	 * @param a
	 * @return OtpErlangObject
	 * @throws BackendException
	 * @throws ConversionException
	 */
	OtpErlangObject rpcx(String m, String f, String signature, Object... a)
			throws RpcException, BackendException;

	/**
	 * @throws ErlangRpcException,
	 *             BackendException
	 * 
	 * @param m
	 * @param f
	 * @param timeout
	 * @param signature
	 *            TODO
	 * @param a
	 * @return OtpErlangObject
	 * @throws BackendException
	 * @throws ConversionException
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

	void addStdListener(IStreamListener dsp);

	String getLabel();

	void setLabel(final String label);

	boolean ping();

	List<IBackendEventListener> getEventListeners(String event);

	String getName();

	String getHost();

	public void setErts(final IProcess process);

	public abstract void initializeErts();

	public void connectAndRegister(final List<ICodeBundle> plugins);
}

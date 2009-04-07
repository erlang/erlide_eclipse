/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package erlang;

import java.util.Collection;
import java.util.Map;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideEdoc {

	public static void files(Collection<String> files,
			Map<String, OtpErlangObject> options) throws RpcException,
			BackendException {
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		OtpErlangObject opts = RpcConverter.encodeMap(options);
		b.call(15000, "edoc", "files", "lsx", files, opts);
	}
}

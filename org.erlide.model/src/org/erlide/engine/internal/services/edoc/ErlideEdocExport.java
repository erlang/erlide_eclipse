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
package org.erlide.engine.internal.services.edoc;

import java.util.Collection;
import java.util.Map;

import org.erlide.engine.services.edoc.EdocExportService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.erlang.TypeConverter;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideEdocExport implements EdocExportService {

    private final IOtpRpc backend;

    public ErlideEdocExport(final IOtpRpc backend) {
        this.backend = backend;
    }

    @Override
    public void files(final Collection<String> files,
            final Map<String, OtpErlangObject> options) throws RpcException {
        final OtpErlangObject opts = TypeConverter.mapToProplist(options);
        backend.call(15000, "edoc", "files", "lsx", files, opts);
    }
}

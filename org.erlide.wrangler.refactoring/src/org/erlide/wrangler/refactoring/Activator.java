/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.engine.util.OtpRpcFactory;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcResult;
import org.erlide.util.ErlLogger;
import org.osgi.framework.BundleContext;

import com.ericsson.otp.erlang.OtpErlangList;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {
    /**
     * The plug-in ID.
     */
    public static final String PLUGIN_ID = "org.erlide.wrangler.refactoring";

    /**
     * The core plugin ID.
     */
    public static final String CORE_ID = "org.erlide.wrangler.core";

    // The shared instance
    private static Activator plugin;

    /**
     * Returns the shared instance
     *
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    /**
     * The constructor
     */
    public Activator() {
    }

    public IOtpRpc getBackend() {
        return OtpRpcFactory.getOtpRpc();
    }

    /**
     * Loads the necessary *.ebin files to the Erlang node for the plug-in.
     *
     * @throws CoreException
     *             detailed exception about the loading process errors
     */
    private void initWrangler() throws CoreException {
        final IOtpRpc mb = getBackend();
        RpcResult res = mb.call_noexception("wrangler_refacs", "init_eclipse", "",
                new Object[0]);
        ErlLogger.debug("Wrangler app started:\n" + res);
        res = mb.call_noexception("wrangler_error_logger", "init", "x",
                new OtpErlangList());

        ErlLogger.debug("Error logger started:" + res);
    }

    @Override
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
        initWrangler();
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
    }

}

package org.erlide.cover.core;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.BundleContext;

public class Activator extends Plugin implements Logger {
	
    // The plug-in ID
    public static final String PLUGIN_ID = "org.erlide.cover.core"; //$NON-NLS-1$

    private static BundleContext context;

	private static Activator plugin;
	
    static BundleContext getContext() {
        return context;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
     * )
     */
    public void start(final BundleContext bundleContext) throws Exception {
        Activator.context = bundleContext;
		plugin = this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    public void stop(final BundleContext bundleContext) throws Exception {
        Activator.context = null;
    }

	public void info(Object msg) {
		log(IStatus.INFO, msg);
	}

	public void error(Object msg) {
		log(IStatus.ERROR, msg);
	}

	private void log(int severity, Object msg) {
		getLog().log(new Status(severity, PLUGIN_ID, String.valueOf(msg)));
	}

	public static Activator getDefault() {
		return plugin;
	}

}

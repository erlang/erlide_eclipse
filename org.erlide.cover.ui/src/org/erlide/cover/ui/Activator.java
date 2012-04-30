package org.erlide.cover.ui;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.cover.core.LogUtils;
import org.erlide.cover.core.Logger;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin implements Logger {

    // The plug-in ID
    public static final String PLUGIN_ID = "org.erlide.cover.ui"; //$NON-NLS-1$

    // The shared instance
    private static Activator plugin;

    /**
     * The constructor
     */
    public Activator() {
    }

    @Override
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
        // CoverBackend.getInstance().addAnnotationMaker(EditorTracker.getInstance());
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
    }

    /**
     * Returns the shared instance
     * 
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    /**
     * Returns an image descriptor for the image file at the given plug-in
     * relative path
     * 
     * @param path
     *            the path
     * @return the image descriptor
     */
    public static ImageDescriptor getImageDescriptor(final String path) {
        return imageDescriptorFromPlugin(PLUGIN_ID, path);
    }

    @Override
    public void info(final Object msg) {
        LogUtils.log(this, IStatus.INFO, msg, 1);
    }

    @Override
    public void error(final Object msg) {
        LogUtils.log(this, IStatus.ERROR, msg, 1);
    }

}

package org.erlide.tracing.core;

import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

    // The plug-in ID
    public static final String PLUGIN_ID = "org.erlide.tracing.core"; //$NON-NLS-1$

    // The shared instance
    private static Activator plugin;

    private static String ICONS_PATH = "icons/";

    public Activator() {
    }

    @Override
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        plugin = this;

        // loading images
        final URL baseUrl = FileLocator.find(getBundle(), new Path(ICONS_PATH),
                null);
        for (final Images image : Images.values()) {
            getImageRegistry().put(
                    image.toString(),
                    ImageDescriptor.createFromURL(new URL(baseUrl
                            + image.getFileName())));
        }
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
    }

    public static Activator getDefault() {
        return plugin;
    }

    /**
     * Returns image stored in registry.
     * 
     * @param image
     * @return image
     */
    public static Image getImage(final Images image) {
        return getDefault().getImageRegistry().get(image.toString());
    }
}

/*******************************************************************************
 * Copyright (c) 2011 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

public enum ErlideImage {

    //@formatter:off
    ALPHAB_SORT(ErlideImagePrefix.T_ELCL, "alphab_sort_co.gif"), 
    TEST_RUNNING(ErlideImagePrefix.T_OBJ, "searchm_obj.gif"),
    TEST_SUCCEEDED(ErlideImagePrefix.T_OBJ, "methpub_obj.gif"),
    TEST_FAILED(ErlideImagePrefix.T_OBJ, "methpri_obj.gif"),
    TEST_SKIPPED(ErlideImagePrefix.T_OBJ, "methpro_obj.gif"),
    CLEAR(ErlideImagePrefix.T_ELCL, "clear_co.gif")
    ;
    //@formatter:on

    private String prefix;
    private String path;

    private static volatile ImageRegistry registry;

    private static URL fgIconBaseURL = null;
    static {
        fgIconBaseURL = ErlideUIPlugin.getDefault().getBundle()
                .getEntry("/icons/full/"); //$NON-NLS-1$
    }

    private ErlideImage(final ErlideImagePrefix prefix, final String path) {
        this.prefix = prefix.getPrefix();
        this.path = path;
    }

    public URL url() throws MalformedURLException {
        return makeIconFileURL(prefix, path);
    }

    public Image getImage() {
        return getRegistry().get(name());
    }

    public ImageDescriptor getDescriptor() {
        return getRegistry().getDescriptor(name());
    }

    public static boolean isInstalled() {
        return registry != null;
    }

    public static synchronized void dispose() {
        registry.dispose();
        registry = null;
    }

    private static ImageRegistry getRegistry() {
        if (registry == null) {
            synchronized (ErlideImage.class) {
                if (registry == null) {
                    registry = new ImageRegistry(Display.getDefault());
                    for (final ErlideImage key : values()) {
                        ImageDescriptor descriptor;
                        try {
                            final URL url = key.url();
                            descriptor = ImageDescriptor.createFromURL(url);
                        } catch (final MalformedURLException e) {
                            descriptor = ImageDescriptor
                                    .getMissingImageDescriptor();
                        }
                        registry.put(key.name(), descriptor);
                    }
                }
            }
        }
        return registry;
    }

    private static URL makeIconFileURL(final String prefix, final String name)
            throws MalformedURLException {
        if (fgIconBaseURL == null) {
            throw new MalformedURLException();
        }

        final StringBuilder buffer = new StringBuilder(prefix);
        buffer.append('/');
        buffer.append(name);
        return new URL(fgIconBaseURL, buffer.toString());
    }

}

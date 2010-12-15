package org.erlide.core.platform;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IRegistryEventListener;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;

public class PlatformChangeListener implements IResourceChangeListener,
        IPreferenceChangeListener, IRegistryEventListener {

    public PlatformChangeListener() {
        final IWorkspace wspace = ResourcesPlugin.getWorkspace();
        wspace.addResourceChangeListener(this);

        // preferencechangelistener is registered per preference node...
        // which ones do we listen to?

        final IExtensionRegistry registry = Platform.getExtensionRegistry();
        registry.addListener(this);
    }

    public void dispose() {
        final IWorkspace wspace = ResourcesPlugin.getWorkspace();
        wspace.removeResourceChangeListener(this);

        // preferencechangelistener is registered per preference node...

        final IExtensionRegistry registry = Platform.getExtensionRegistry();
        registry.removeListener(this);
    }

    public void resourceChanged(final IResourceChangeEvent event) {
        // System.out.println("### resources changed: " + event);
    }

    public void preferenceChange(final PreferenceChangeEvent event) {
        // System.out.println("### preferences changed: " + event);
    }

    public void added(final IExtension[] extensions) {
        // System.out.println("### extensions added: " + extensions);
    }

    public void added(final IExtensionPoint[] extensionPoints) {
        // System.out.println("### extensionPoints added: " + extensionPoints);
    }

    public void removed(final IExtension[] extensions) {
        // System.out.println("### extensions removed: " + extensions);
    }

    public void removed(final IExtensionPoint[] extensionPoints) {
        // System.out.println("### extensionPoints removed: " +
        // extensionPoints);
    }

}

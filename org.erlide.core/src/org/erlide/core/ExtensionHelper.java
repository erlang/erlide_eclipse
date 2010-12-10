/*
 * Created on 21/08/2005
 */
package org.erlide.core;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.google.common.collect.Lists;

public class ExtensionHelper {

    /**
     * This should be used to add participants at test-time. It should be the
     * name of the extension point to a list (which will be returned)
     */
    public static Map<String, List<Object>> testingParticipants;

    private static Map<String, IExtension[]> extensionsCache = new HashMap<String, IExtension[]>();

    // TODO list the extension points
    public static final String EDITOR_LISTENER = "org.erlide.editor_listener";

    public static IExtension[] getExtensions(final String type) {
        IExtension[] extensions = extensionsCache.get(type);
        if (extensions == null) {
            final IExtensionRegistry registry = Platform.getExtensionRegistry();
            if (registry != null) { // we may not be in eclipse env when testing
                try {
                    final IExtensionPoint extensionPoint = registry
                            .getExtensionPoint(type);
                    extensions = extensionPoint.getExtensions();
                    extensionsCache.put(type, extensions);
                } catch (final Exception e) {
                    ErlangPlugin.log("Error getting extension for:" + type, e);
                    throw new RuntimeException(e);
                }
            } else {
                extensions = new IExtension[0];
            }
        }
        return extensions;
    }

    @SuppressWarnings("unchecked")
    public static Object getParticipant(final String type) {
        // only one participant may be used for this
        final List<Object> participants = getParticipants(type);
        if (participants.size() == 1) {
            return participants.get(0);
        }

        if (participants.size() == 0) {
            return null;
        }

        if (participants.size() > 1) {
            throw new RuntimeException(
                    "More than one participant is registered for type:" + type);
        }

        throw new RuntimeException("Should never get here!");

    }

    /**
     * @param type
     *            the extension we want to get
     * @return a list of classes created from those extensions
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static List getParticipants(final String type) {
        if (testingParticipants != null) {
            return testingParticipants.get(type);
        }

        final List list = Lists.newArrayList();
        final IExtension[] extensions = getExtensions(type);
        // For each extension ...
        for (int i = 0; i < extensions.length; i++) {
            final IExtension extension = extensions[i];
            final IConfigurationElement[] elements = extension
                    .getConfigurationElements();
            // For each member of the extension ...
            for (int j = 0; j < elements.length; j++) {
                final IConfigurationElement element = elements[j];

                try {
                    list.add(element.createExecutableExtension("class"));
                } catch (final Exception e) {
                    ErlangPlugin.getDefault().log(e);
                }
            }
        }
        return list;
    }
}

package org.erlide.cover.api;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.erlide.core.ErlangPlugin;

/**
 * Public API for Cover plug-in
 * @author Krzysztof Goj
 */
public class CoverageAnalysis {
    
    /**
     * @return true if the Cover plug-in is installed.
     */
    public static boolean isAvailable() {
        return getCoveragePerformer() != null;
    }
    
    /**
     * Intended to be run before running the tests.
     * 
     * @throws UnsupportedOperationException when Cover plug-in is not installed
     * @throws CoverException on Cover-specific failures
     */
    public static void prepareAnalysis(List<String> nodes, IConfiguration configuration) throws CoverException {
        ICoveragePerformer performer = getCoveragePerformerOrThrow();
        performer.startCover(nodes);
        performer.setCoverageConfiguration(configuration);
    }

    /**
     * Intended to be run after running the tests.
     * Performs the actual analysis and displays the results to the user.
     * 
     * @throws UnsupportedOperationException when Cover plug-in is not installed
     * @throws CoverException on Cover-specific failures
     */
    public static void performAnalysis() throws CoverException {
        ICoveragePerformer performer = getCoveragePerformerOrThrow();
        performer.analyse();
    }

    private static ICoveragePerformer getCoveragePerformerOrThrow() {
        ICoveragePerformer result = getCoveragePerformer();
        if (result == null)
            throw new UnsupportedOperationException();
        return result;
    }

    private static ICoveragePerformer getCoveragePerformer() {
        IConfigurationElement[] conf = Platform.getExtensionRegistry().getConfigurationElementsFor(ICoveragePerformerProxy.ID);
        ICoveragePerformerProxy proxy = null;
        try {
            for (IConfigurationElement ce: conf) {
                if (proxy != null)
                    throw new RuntimeException("There should be only one CoveragePerformerProxy implementation");
                proxy = (ICoveragePerformerProxy) ce.createExecutableExtension("class");
            }
        } catch (CoreException e) {
            ErlangPlugin.log("Exception when trying to get Coverage Performer", e);
        }
        return proxy.getPerformer();
    }

}

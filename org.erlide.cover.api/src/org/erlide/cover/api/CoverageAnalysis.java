package org.erlide.cover.api;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.erlide.core.ErlangPlugin;

/**
 * Public API for Cover plug-in
 * 
 * Notes: - user should make sure that only one set of tests performs coverage
 * at time, that is: no calls to prepareAnalysis are made between
 * prepareAnalysis() - performAnalysis() call pair.
 * 
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
     * @throws UnsupportedOperationException
     *             when Cover plug-in is not installed
     * @throws CoverException
     *             on Cover-specific failures
     */
    public static void prepareAnalysis(final IConfiguration configuration)
            throws CoverException {
        prepareAnalysis(new ArrayList<String>(), configuration);
    }

    /**
     * Intended to be run before running the tests.
     * 
     * @param additionalNodes
     *            - other nodes (besides Cover's node) that are used to gather
     *            the coverage info
     * 
     * @throws UnsupportedOperationException
     *             when Cover plug-in is not installed
     * @throws CoverException
     *             on Cover-specific failures
     */
    public static void prepareAnalysis(final List<String> additionalNodes,
            final IConfiguration configuration) throws CoverException {
        final ICoveragePerformer performer = getCoveragePerformerOrThrow();
        performer.startCover(additionalNodes);
        performer.setCoverageConfiguration(configuration);
    }

    /**
     * Intended to be run after running the tests. Performs the actual analysis
     * and displays the results to the user.
     * 
     * @throws UnsupportedOperationException
     *             when Cover plug-in is not installed
     * @throws CoverException
     *             on Cover-specific failures
     */
    public static void performAnalysis() throws CoverException {
        getCoveragePerformerOrThrow().analyse();
    }

    private static ICoveragePerformer getCoveragePerformerOrThrow() {
        final ICoveragePerformer result = getCoveragePerformer();
        if (result == null) {
            throw new UnsupportedOperationException();
        }
        return result;
    }

    private static ICoveragePerformer getCoveragePerformer() {
        final IConfigurationElement[] conf = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(ICoveragePerformerProxy.ID);
        ICoveragePerformerProxy proxy = null;
        try {
            for (final IConfigurationElement ce : conf) {
                if (proxy != null) {
                    throw new RuntimeException(
                            "There should be only one CoveragePerformerProxy implementation");
                }
                proxy = (ICoveragePerformerProxy) ce
                        .createExecutableExtension("class");
            }
        } catch (final CoreException e) {
            ErlangPlugin.log("Exception when trying to get Coverage Performer",
                    e);
        }
        return proxy.getPerformer();
    }

}

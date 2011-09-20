package org.erlide.cover.api;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

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
        try {
            return getCoveragePerformer() != null;
        } catch (final CoreException e) {
            e.printStackTrace();
            return false;
        }
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
        try {
            final ICoveragePerformer performer = getCoveragePerformerOrThrow();
            performer.startCover(additionalNodes);
            performer.setCoverageConfiguration(configuration);
        } catch (final CoreException e) {
            throw new CoverException(e);
        }
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
        try {
            getCoveragePerformerOrThrow().analyse();
        } catch (final CoreException e) {
            throw new CoverException(e);
        }
    }

    /**
     * Intended to obtain access to Cover node, that coverage analysis could be
     * performed on.
     * 
     * @return Cover node
     * @throws CoverException
     *             on Cover-specific failures
     */
    public static ICoverBackend getBackend() throws CoverException {
        final IConfigurationElement[] conf = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(ICoveragePerformerProxy.ID);
        ICoveragePerformerProxy proxy = null;
        for (final IConfigurationElement ce : conf) {
            if (proxy != null) {
                throw new RuntimeException(
                        "There should be only one CoveragePerformerProxy implementation");
            }
            try {
                proxy = (ICoveragePerformerProxy) ce
                        .createExecutableExtension("class");
            } catch (final CoreException e) {
                e.printStackTrace();
                throw new CoverException(e);
            }
        }
        return proxy != null ? proxy.getBackend() : null;
    }

    private static ICoveragePerformer getCoveragePerformerOrThrow()
            throws CoreException {
        final ICoveragePerformer result = getCoveragePerformer();
        if (result == null) {
            throw new UnsupportedOperationException();
        }
        return result;
    }

    private static ICoveragePerformer getCoveragePerformer()
            throws CoreException {
        final IConfigurationElement[] conf = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(ICoveragePerformerProxy.ID);
        ICoveragePerformerProxy proxy = null;
        for (final IConfigurationElement ce : conf) {
            if (proxy != null) {
                throw new RuntimeException(
                        "There should be only one CoveragePerformerProxy implementation");
            }
            proxy = (ICoveragePerformerProxy) ce
                    .createExecutableExtension("class");
        }
        return proxy != null ? proxy.getPerformer() : null;
    }

}

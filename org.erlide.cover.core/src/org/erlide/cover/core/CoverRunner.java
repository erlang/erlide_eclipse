package org.erlide.cover.core;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Semaphore;

import org.eclipse.core.runtime.IPath;
import org.erlide.cover.api.AbstractCoverRunner;
import org.erlide.cover.api.CoverageAnalysis;
import org.erlide.cover.api.IConfiguration;
import org.erlide.cover.constants.TestConstants;
import org.erlide.cover.views.model.TestTreeModel;
import org.erlide.jinterface.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;

/**
 * Class for launching cover
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoverRunner extends AbstractCoverRunner {

    private final Logger log; // logger
    private static Semaphore semaphore = new Semaphore(1);

    public CoverRunner() {
        log = Activator.getDefault();
    }

    @Override
    public void run() {
        final CoverBackend backend = CoverBackend.getInstance();
        try {

            semaphore.acquireUninterruptibly();
            final IConfiguration config = backend.getSettings().getConfig();
            CoverageAnalysis.prepareAnalysis(config);
            runTests(config);
            CoverageAnalysis.performAnalysis();

        } catch (final Exception e) {
            e.printStackTrace();
            backend.handleError("Exception while running cover: " + e);
        } finally {
            semaphore.release();
        }
    }

    // performs tests
    private void runTests(final IConfiguration config) throws RpcException {

        // clear viewer
        TestTreeModel.getInstance().clear();
        for (final IEUnitObserver obs : CoverBackend.getInstance()
                .getEUnitListeners()) {
            obs.treeChanged();
            obs.labelChanged();
        }

        // test
        CoverBackend
                .getInstance()
                .getBackend()
                .call(TestConstants.TEST_ERL_BACKEND,
                        TestConstants.FUN_START,
                        "x",
                        new OtpErlangAtom(CoverBackend.getInstance()
                                .getSettings().getFramework()));

        log.info(config.getProject().getWorkspaceProject().getLocation());
        final IPath ppath = config.getProject().getWorkspaceProject()
                .getLocation();
        log.info(ppath.append(config.getOutputDir()));

        CoverBackend
                .getInstance()
                .getBackend()
                .call(TestConstants.TEST_ERL_BACKEND,
                        TestConstants.FUN_OUTPUT_DIR, "s",
                        ppath.append(config.getOutputDir()).toString());

        switch (CoverBackend.getInstance().getSettings().getType()) {
        case MODULE:

            log.info(config.getModules().iterator().next().getFilePath());

            CoverBackend
                    .getInstance()
                    .getBackend()
                    .call(TestConstants.TEST_ERL_BACKEND,
                            TestConstants.FUN_TEST,
                            "ss",
                            CoverBackend.getInstance().getSettings().getType()
                                    .name().toLowerCase(),
                            config.getModules().iterator().next().getFilePath());
            break;
        case ALL:
            final List<String> testDirs = new LinkedList<String>();
            for (final IPath p : config.getSourceDirs()) {
                log.info(p);
                if (!p.toString().endsWith("test")) {
                    testDirs.add(ppath.append(p).append("test").toString());
                }
            }
            testDirs.add(ppath.append("test").toString());

            for (final String path : testDirs) {
                log.info(path);

                CoverBackend
                        .getInstance()
                        .getBackend()
                        .call(TestConstants.TEST_ERL_BACKEND,
                                TestConstants.FUN_TEST,
                                "ss",
                                CoverBackend.getInstance().getSettings()
                                        .getType().name().toLowerCase(), path);
            }
            break;
        default:
            break;
        }
    }

}

package org.erlide.cover.core;

import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.backend.BackendException;
import org.erlide.cover.constants.TestConstants;
import org.erlide.cover.core.api.CoveragePerformer;
import org.erlide.cover.core.api.IConfiguration;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Class for launching cover
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoverRunner extends Thread {

    // private final CoverBackend backend;
    private CoveragePerformer perf;

    private Logger log; // logger

    public CoverRunner() {
        perf = CoveragePerformer.getPerformer();
        log = Logger.getLogger(getClass());
    }

    @Override
    public void run() {

        try {

            IConfiguration config = CoverBackend.getInstance().getSettings()
                    .getConfig();

            perf.startCover(new LinkedList<String>());

            // start tests

            OtpErlangObject res = CoverBackend
                    .getInstance()
                    .getBackend()
                    .call(TestConstants.TEST_ERL_BACKEND,
                            TestConstants.FUN_START,
                            "x",
                            new OtpErlangAtom(CoverBackend.getInstance()
                                    .getSettings().getFramework()));

            // TODO handle res

            log.debug(config.getProject().getWorkspaceProject().getLocation());
            IPath ppath = config.getProject().getWorkspaceProject().getLocation();
            log.debug(ppath.append(config.getOutputDir()));

            res = CoverBackend
                    .getInstance()
                    .getBackend()
                    .call(TestConstants.TEST_ERL_BACKEND,
                            TestConstants.FUN_OUTPUT_DIR, "s",
                            ppath.append(config.getOutputDir()).toString());

            // TODO handle res

            perf.setCoverageConfiguration(config);

            switch (CoverBackend.getInstance().getSettings().getType()) {
            case MODULE:

                log.debug(config.getModules().iterator().next().getFilePath());

                CoverBackend
                        .getInstance()
                        .getBackend()
                        .call(TestConstants.TEST_ERL_BACKEND,
                                TestConstants.FUN_TEST,
                                "ss",
                                CoverBackend.getInstance().getSettings()
                                        .getType().name().toLowerCase(),
                                config.getModules().iterator().next()
                                        .getFilePath());
                break;
            case ALL:
                List<String> testDirs = new LinkedList<String>();
                for (IPath p : config.getSourceDirs()) {
                    log.debug(p);
                    if (!p.toString().endsWith("test")) // !
                        testDirs.add(ppath.append(p).append("test").toString());
                }
                testDirs.add(ppath.append("test").toString());

                for (String path : testDirs) {
                    log.debug(path);

                    CoverBackend
                            .getInstance()
                            .getBackend()
                            .call(TestConstants.TEST_ERL_BACKEND,
                                    TestConstants.FUN_TEST,
                                    "ss",
                                    CoverBackend.getInstance().getSettings()
                                            .getType().name().toLowerCase(),
                                    path);
                }
                break;
            default:
                break;
            }

            perf.analyse();

        } catch (CoverException e) {
            e.printStackTrace();
            CoverBackend.getInstance().handleError(
                    "Exception while running cover" + e);
        } catch (BackendException e) {
            e.printStackTrace();
            CoverBackend.getInstance().handleError(
                    "Exception while running tests" + e);
        }

        CoverBackend.getInstance().coverageFinished();
    }

}

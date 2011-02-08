package org.erlide.cover.core;

import org.apache.log4j.Logger;
import org.erlide.cover.constants.Constants;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.jinterface.backend.BackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Class for launching cover
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoverRunner extends Thread {

    private final CoverBackend backend;

    private Logger log;     // logger
    
    public CoverRunner(final CoverBackend backend) {
        this.backend = backend;
        log = Logger.getLogger(getClass());
    }

    @Override
    public void run() {

        final StatsTreeModel model = StatsTreeModel.getInstance();
        model.clear();
        backend.getAnnotationMaker().clearAllAnnotations();
        for (final ICoverObserver obs : backend.getListeners()) {
            obs.eventOccured(new CoverEvent(CoverStatus.UPDATE));
        }

        try {
            final OtpErlangObject res = backend.getBackend().call(
                    Constants.ERLANG_BACKEND, Constants.FUN_START, "x",
                    new OtpErlangAtom(backend.getSettings().getFramework()));
            if (res instanceof OtpErlangTuple) {
                final OtpErlangTuple resTuple = (OtpErlangTuple) res;
                if (!(resTuple.elementAt(0).toString().equals("ok") || resTuple
                        .elementAt(0).toString().equals("error")
                        && resTuple.elementAt(1) instanceof OtpErlangTuple
                        && ((OtpErlangTuple) resTuple.elementAt(1))
                                .elementAt(0).toString()
                                .equals("already_started"))) {
                    log.debug("cover_error");
                    return;
                }
            } else {
                return;
            }

            // the loop is a preparation for possible custom configuration
            for (final CoverObject obj : backend.getSettings().objects()) {

                backend.getBackend().call(Constants.ERLANG_BACKEND,
                        Constants.FUN_BEAM_DIR, "s", obj.getPathEbin());

                // TODO: change the way of obtaining reports (that it can serve
                // many objects)
                OtpErlangObject htmlPath;
                if (obj.getType() == CoverObject.MODULE) {
                    htmlPath = backend.getBackend().call(
                            Constants.ERLANG_BACKEND, Constants.FUN_COVER_PREP,
                            "sss", backend.getSettings().getTypeAsString(),
                            obj.getName(), obj.getPathTst()); // tst = src
                } else {
                    htmlPath = backend.getBackend().call(
                            Constants.ERLANG_BACKEND, Constants.FUN_COVER_PREP,
                            "sss", backend.getSettings().getTypeAsString(),
                            obj.getPathSrc(), obj.getPathTst());
                }

                model.setIndex(htmlPath.toString().substring(1,
                        htmlPath.toString().length() - 1));

            }

        } catch (final BackendException e) {
            e.printStackTrace();
            backend.handleError("Exception while running cover occured: " + e);
        }

        // TODO: index should be created here.

        backend.coverageFinished();
    }

}

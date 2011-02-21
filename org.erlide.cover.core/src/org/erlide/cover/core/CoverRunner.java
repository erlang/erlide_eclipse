package org.erlide.cover.core;

import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.erlide.cover.core.api.CoveragePerformer;

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

        /*
         * old
         * 
         * final StatsTreeModel model = StatsTreeModel.getInstance();
         * model.clear(); backend.getAnnotationMaker().clearAllAnnotations();
         * for (final ICoverObserver obs : backend.getListeners()) {
         * obs.eventOccured(new CoverEvent(CoverStatus.UPDATE)); }
         * 
         * try { final OtpErlangObject res = backend.getBackend().call(
         * Constants.ERLANG_BACKEND, Constants.FUN_START, "x", new
         * OtpErlangAtom(backend.getSettings().getFramework())); if (res
         * instanceof OtpErlangTuple) { final OtpErlangTuple resTuple =
         * (OtpErlangTuple) res; if
         * (!(resTuple.elementAt(0).toString().equals("ok") || resTuple
         * .elementAt(0).toString().equals("error") && resTuple.elementAt(1)
         * instanceof OtpErlangTuple && ((OtpErlangTuple) resTuple.elementAt(1))
         * .elementAt(0).toString() .equals("already_started"))) {
         * log.debug("cover_error"); return; } } else { return; }
         * 
         * // the loop is a preparation for possible custom configuration for
         * (final CoverObject obj : backend.getSettings().objects()) {
         * 
         * backend.getBackend().call(Constants.ERLANG_BACKEND,
         * Constants.FUN_BEAM_DIR, "s", obj.getPathEbin());
         * 
         * // TODO: change the way of obtaining reports (that it can serve //
         * many objects) OtpErlangObject htmlPath; if (obj.getType() ==
         * CoverObject.MODULE) { htmlPath = backend.getBackend().call(
         * Constants.ERLANG_BACKEND, Constants.FUN_COVER_PREP, "sss",
         * backend.getSettings().getTypeAsString(), obj.getName(),
         * obj.getPathTst()); // tst = src } else { htmlPath =
         * backend.getBackend().call( Constants.ERLANG_BACKEND,
         * Constants.FUN_COVER_PREP, "sss",
         * backend.getSettings().getTypeAsString(), obj.getPathSrc(),
         * obj.getPathTst()); }
         * 
         * model.setIndex(htmlPath.toString().substring(1,
         * htmlPath.toString().length() - 1));
         * 
         * }
         */

        // new
        try {
            perf.startCover(new LinkedList<String>());
            perf.setCoverageConfiguration(CoverBackend.getInstance()
                    .getSettings().getConfig());

            // launch testing

            perf.analyse();
        } catch (CoverException e) {
            // TODO
            e.printStackTrace();
        }

        // } catch (final BackendException e) {
        // e.printStackTrace();
        // backend.handleError("Exception while running cover occured: " + e);
        // }

        // TODO: index should be created here.

        CoverBackend.getInstance().coverageFinished();
    }

}

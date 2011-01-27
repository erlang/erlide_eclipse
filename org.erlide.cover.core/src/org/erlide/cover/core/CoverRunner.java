package org.erlide.cover.core;

import org.erlide.cover.constants.Constants;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Class for launching cover
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoverRunner extends Thread {

    private CoverBackend backend;

    public CoverRunner(CoverBackend backend) {
        this.backend = backend;
    }

    public void run() {

        StatsTreeModel model = StatsTreeModel.getInstance();
        model.clear();
        backend.getAnnotationMaker().clearAllAnnotations();
        for (ICoverObserver obs : backend.getListeners())
            obs.eventOccured(new CoverEvent(CoverStatus.UPDATE));

        try {
            backend.getBackend().call(Constants.ERLANG_BACKEND,
                    Constants.FUN_START, "x",
                    new OtpErlangAtom(backend.getSettings().getFramework()));
        } catch (BackendException e) {
            e.printStackTrace();
            backend.handleError("Exception while running cover occured: " + e);
        }

        // the loop is a preparation for possible custom configuration
        for (CoverObject obj : backend.getSettings().objects()) {

            System.out.println("Starting cover..");

            try {
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
                System.out.println("Called");
                System.out.println(backend.getSettings().getTypeAsString());

                System.out.println(htmlPath);
                model.setIndex(htmlPath.toString().substring(1,
                        htmlPath.toString().length() - 1));
                
                backend.getBackend().call(Constants.ERLANG_BACKEND, 
                        Constants.FUN_STOP, "");
                
            } catch (BackendException e) {
                e.printStackTrace();
                backend.handleError("Exception while running cover occured: " + e);
            }
        }
        
        
        // TODO: index should be created here.

        backend.coverageFinished();
    }

}

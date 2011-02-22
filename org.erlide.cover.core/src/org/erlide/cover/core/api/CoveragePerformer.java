package org.erlide.cover.core.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.Logger;
import org.erlide.core.erlang.IErlModule;
import org.erlide.cover.constants.CoverConstants;
import org.erlide.cover.core.CoverBackend;
import org.erlide.cover.core.CoverEvent;
import org.erlide.cover.core.CoverException;
import org.erlide.cover.core.CoverStatus;
import org.erlide.cover.core.ICoverObserver;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.jinterface.backend.BackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class CoveragePerformer implements CoverAPI {

    private static CoveragePerformer performer;

    private Collection<String> coverNodes;
    private IConfiguration config;

    private Logger log; // logger

    private CoveragePerformer() {
        log = Logger.getLogger(this.getClass());
        coverNodes = new LinkedList<String>();
    }

    public static synchronized CoveragePerformer getPerformer() {
        if (performer == null)
            performer = new CoveragePerformer();
        return performer;
    }

    public synchronized void startCover(Collection<String> nodes)
            throws CoverException {

        final StatsTreeModel model = StatsTreeModel.getInstance();
        model.clear();
        CoverBackend.getInstance().getAnnotationMaker().clearAllAnnotations();

        for (final ICoverObserver obs : CoverBackend.getInstance()
                .getListeners()) {
            obs.eventOccured(new CoverEvent(CoverStatus.UPDATE));
        }

        boolean different = false;
        for (String node : nodes)
            if (!coverNodes.contains(node)) {
                different = true;
                break;
            }

        if (coverNodes.isEmpty() || different) {

            coverNodes = nodes;
            log.info(CoverBackend.getInstance().getBackend().getFullNodeName());
            coverNodes.add(CoverBackend.getInstance().getBackend()
                    .getFullNodeName());

            // TODO: restarting

            List<OtpErlangObject> names = new ArrayList<OtpErlangObject>(
                    coverNodes.size());
            for (String name : coverNodes) {
                names.add(new OtpErlangAtom(name));
            }

            OtpErlangList nodesList = new OtpErlangList(
                    names.toArray(new OtpErlangObject[0]));

            try {
                final OtpErlangObject res = CoverBackend
                        .getInstance()
                        .getBackend()
                        .call(CoverConstants.COVER_ERL_BACKEND,
                                CoverConstants.FUN_START, "x", nodesList);

                // TODO: check if res is ok

            } catch (BackendException e) {
                e.printStackTrace();
                throw new CoverException(e.getMessage());
            }

        }

    }

    public synchronized void setCoverageConfiguration(IConfiguration conf)
            throws CoverException {
        config = conf;

        // preparation - cover compilation
        List<OtpErlangObject> paths = new ArrayList<OtpErlangObject>(config
                .getModules().size());
        for (IErlModule module : config.getModules()) {
            log.debug(module.getFilePath());
            paths.add(new OtpErlangList(module.getFilePath()));
        }

        try {
            OtpErlangObject res = CoverBackend
                    .getInstance()
                    .getBackend()
                    .call(CoverConstants.COVER_ERL_BACKEND,
                            CoverConstants.FUN_PREP, "x", paths);

            // TODO check the res
        } catch (BackendException e) {
            e.printStackTrace();
            throw new CoverException(e.getMessage());
        }

    }

    public synchronized void analyse() throws CoverException {

        List<OtpErlangObject> modules = new ArrayList<OtpErlangObject>(config
                .getModules().size());
        for (IErlModule module : config.getModules()) {
            log.debug(module.getModuleName());
            modules.add(new OtpErlangList(module.getModuleName()));
        }

        try {
            OtpErlangObject res = CoverBackend
                    .getInstance()
                    .getBackend()
                    .call(CoverConstants.COVER_ERL_BACKEND,
                            CoverConstants.FUN_ANALYSE, "x", modules);
            
            
            if(res instanceof OtpErlangAtom && res.toString().equals("no_file"))
                return;  // do sth more then??
            
            final StatsTreeModel model = StatsTreeModel.getInstance();
            model.setIndex(res.toString().substring(1,
                    res.toString().length() - 1));

        } catch (BackendException e) {
            e.printStackTrace();
            throw new CoverException(e.getMessage());
        }
    }

}
